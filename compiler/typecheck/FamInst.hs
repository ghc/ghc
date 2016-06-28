-- The @FamInst@ type: family instance heads

{-# LANGUAGE CPP, GADTs #-}

module FamInst (
        FamInstEnvs, tcGetFamInstEnvs,
        checkFamInstConsistency, tcExtendLocalFamInstEnv,
        tcLookupDataFamInst, tcLookupDataFamInst_maybe,
        tcInstNewTyCon_maybe, tcTopNormaliseNewTypeTF_maybe,
        newFamInst,

        -- * Injectivity
        makeInjectivityErrors
    ) where

import HscTypes
import FamInstEnv
import InstEnv( roughMatchTcs )
import Coercion
import TcEvidence
import LoadIface
import TcRnMonad
import SrcLoc
import TyCon
import TcType
import CoAxiom
import DynFlags
import Module
import Outputable
import Util
import RdrName
import DataCon ( dataConName )
import Maybes
import Type
import TyCoRep
import TcMType
import Name
import Pair
import Panic
import VarSet
import Bag( Bag, unionBags, unitBag )
import Control.Monad
import Unique
import Data.Set (Set)
import qualified Data.Set as Set

#include "HsVersions.h"

{-
************************************************************************
*                                                                      *
                 Making a FamInst
*                                                                      *
************************************************************************
-}

-- All type variables in a FamInst must be fresh. This function
-- creates the fresh variables and applies the necessary substitution
-- It is defined here to avoid a dependency from FamInstEnv on the monad
-- code.

newFamInst :: FamFlavor -> CoAxiom Unbranched -> TcRnIf gbl lcl FamInst
-- Freshen the type variables of the FamInst branches
-- Called from the vectoriser monad too, hence the rather general type
newFamInst flavor axiom@(CoAxiom { co_ax_tc = fam_tc })
  = ASSERT2( tyCoVarsOfTypes lhs `subVarSet` tcv_set, text "lhs" <+> pp_ax )
    ASSERT2( tyCoVarsOfType  rhs `subVarSet` tcv_set, text "rhs" <+> pp_ax )
    ASSERT2( lhs_kind `eqType` rhs_kind, text "kind" <+> pp_ax $$ ppr lhs_kind $$ ppr rhs_kind )
    do { (subst, tvs') <- freshenTyVarBndrs tvs
       ; (subst, cvs') <- freshenCoVarBndrsX subst cvs
       ; return (FamInst { fi_fam      = tyConName fam_tc
                         , fi_flavor   = flavor
                         , fi_tcs      = roughMatchTcs lhs
                         , fi_tvs      = tvs'
                         , fi_cvs      = cvs'
                         , fi_tys      = substTys subst lhs
                         , fi_rhs      = substTy  subst rhs
                         , fi_axiom    = axiom }) }
  where
    lhs_kind = typeKind (mkTyConApp fam_tc lhs)
    rhs_kind = typeKind rhs
    tcv_set  = mkVarSet (tvs ++ cvs)
    pp_ax    = pprCoAxiom axiom
    CoAxBranch { cab_tvs = tvs
               , cab_cvs = cvs
               , cab_lhs = lhs
               , cab_rhs = rhs } = coAxiomSingleBranch axiom


{-
************************************************************************
*                                                                      *
        Optimised overlap checking for family instances
*                                                                      *
************************************************************************

Note [Checking family instance consistency]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For any two family instance modules that we import directly or indirectly, we
check whether the instances in the two modules are consistent, *unless* we can
be certain that the instances of the two modules have already been checked for
consistency during the compilation of modules that we import.

Why do we need to check?  Consider
   module X1 where                module X2 where
    data T1                         data T2
    type instance F T1 b = Int      type instance F a T2 = Char
    f1 :: F T1 a -> Int             f2 :: Char -> F a T2
    f1 x = x                        f2 x = x

Now if we import both X1 and X2 we could make (f2 . f1) :: Int -> Char.
Notice that neither instance is an orphan.

How do we know which pairs of modules have already been checked?  Any pair of
modules where both modules occur in the `HscTypes.dep_finsts' set (of the
`HscTypes.Dependencies') of one of our directly imported modules must have
already been checked.  Everything else, we check now.  (So that we can be
certain that the modules in our `HscTypes.dep_finsts' are consistent.)
-}

-- The optimisation of overlap tests is based on determining pairs of modules
-- whose family instances need to be checked for consistency.
--
data ModulePair = ModulePair Module Module
                  -- Invariant: first Module < second Module
                  -- use the smart constructor

-- | Smart constructor that establishes the invariant
modulePair :: Module -> Module -> ModulePair
modulePair a b
  | a < b = ModulePair a b
  | otherwise = ModulePair b a

instance Eq ModulePair where
  (ModulePair a1 b1) == (ModulePair a2 b2) = a1 == a2 && b1 == b2

instance Ord ModulePair where
  (ModulePair a1 b1) `compare` (ModulePair a2 b2) =
    nonDetCmpModule a1 a2 `thenCmp`
    nonDetCmpModule b1 b2
    -- See Note [ModulePairSet determinism and performance]

instance Outputable ModulePair where
  ppr (ModulePair m1 m2) = angleBrackets (ppr m1 <> comma <+> ppr m2)

-- Fast, nondeterministic comparison on Module. Don't use when the ordering
-- can change the ABI. See Note [ModulePairSet determinism and performance]
nonDetCmpModule :: Module -> Module -> Ordering
nonDetCmpModule a b =
  nonDetCmpUnique (getUnique $ moduleUnitId a) (getUnique $ moduleUnitId b)
  `thenCmp`
  nonDetCmpUnique (getUnique $ moduleName a) (getUnique $ moduleName b)

type ModulePairSet = Set ModulePair
{-
Note [ModulePairSet determinism and performance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The size of ModulePairSet is quadratic in the number of modules.
The Ord instance for Module uses string comparison which is linear in the
length of ModuleNames and UnitIds. This adds up to a significant cost, see
#12191.

To get reasonable performance ModulePairSet uses nondeterministic ordering
on Module based on Uniques. It doesn't affect the ABI, because it only
determines the order the modules are checked for family instance consistency.
See Note [Unique Determinism] in Unique
-}

listToSet :: [ModulePair] -> ModulePairSet
listToSet l = Set.fromList l

checkFamInstConsistency :: [Module] -> [Module] -> TcM ()
-- See Note [Checking family instance consistency]
checkFamInstConsistency famInstMods directlyImpMods
  = do { dflags     <- getDynFlags
       ; (eps, hpt) <- getEpsAndHpt
       ; let { -- Fetch the iface of a given module.  Must succeed as
               -- all directly imported modules must already have been loaded.
               modIface mod =
                 case lookupIfaceByModule dflags hpt (eps_PIT eps) mod of
                   Nothing    -> panicDoc "FamInst.checkFamInstConsistency"
                                          (ppr mod $$ pprHPT hpt)
                   Just iface -> iface

             ; hmiModule     = mi_module . hm_iface
             ; hmiFamInstEnv = extendFamInstEnvList emptyFamInstEnv
                               . md_fam_insts . hm_details
             ; hpt_fam_insts = mkModuleEnv [ (hmiModule hmi, hmiFamInstEnv hmi)
                                           | hmi <- eltsHpt hpt]
             ; groups        = map (dep_finsts . mi_deps . modIface)
                                   directlyImpMods
             ; okPairs       = listToSet $ concatMap allPairs groups
                 -- instances of okPairs are consistent
             ; criticalPairs = listToSet $ allPairs famInstMods
                 -- all pairs that we need to consider
             ; toCheckPairs  =
                 Set.elems $ criticalPairs `Set.difference` okPairs
                 -- the difference gives us the pairs we need to check now
                 -- See Note [ModulePairSet determinism and performance]
             }

       ; mapM_ (check hpt_fam_insts) toCheckPairs
       }
  where
    allPairs []     = []
    allPairs (m:ms) = map (modulePair m) ms ++ allPairs ms

    check hpt_fam_insts (ModulePair m1 m2)
      = do { env1 <- getFamInsts hpt_fam_insts m1
           ; env2 <- getFamInsts hpt_fam_insts m2
           ; mapM_ (checkForConflicts (emptyFamInstEnv, env2))
                   (famInstEnvElts env1)
           ; mapM_ (checkForInjectivityConflicts (emptyFamInstEnv,env2))
                   (famInstEnvElts env1)
 }


getFamInsts :: ModuleEnv FamInstEnv -> Module -> TcM FamInstEnv
getFamInsts hpt_fam_insts mod
  | Just env <- lookupModuleEnv hpt_fam_insts mod = return env
  | otherwise = do { _ <- initIfaceTcRn (loadSysInterface doc mod)
                   ; eps <- getEps
                   ; return (expectJust "checkFamInstConsistency" $
                             lookupModuleEnv (eps_mod_fam_inst_env eps) mod) }
  where
    doc = ppr mod <+> text "is a family-instance module"

{-
************************************************************************
*                                                                      *
        Lookup
*                                                                      *
************************************************************************

-}

-- | If @co :: T ts ~ rep_ty@ then:
--
-- > instNewTyCon_maybe T ts = Just (rep_ty, co)
--
-- Checks for a newtype, and for being saturated
-- Just like Coercion.instNewTyCon_maybe, but returns a TcCoercion
tcInstNewTyCon_maybe :: TyCon -> [TcType] -> Maybe (TcType, TcCoercion)
tcInstNewTyCon_maybe = instNewTyCon_maybe

-- | Like 'tcLookupDataFamInst_maybe', but returns the arguments back if
-- there is no data family to unwrap.
-- Returns a Representational coercion
tcLookupDataFamInst :: FamInstEnvs -> TyCon -> [TcType]
                    -> (TyCon, [TcType], Coercion)
tcLookupDataFamInst fam_inst_envs tc tc_args
  | Just (rep_tc, rep_args, co)
      <- tcLookupDataFamInst_maybe fam_inst_envs tc tc_args
  = (rep_tc, rep_args, co)
  | otherwise
  = (tc, tc_args, mkRepReflCo (mkTyConApp tc tc_args))

tcLookupDataFamInst_maybe :: FamInstEnvs -> TyCon -> [TcType]
                          -> Maybe (TyCon, [TcType], Coercion)
-- ^ Converts a data family type (eg F [a]) to its representation type (eg FList a)
-- and returns a coercion between the two: co :: F [a] ~R FList a.
tcLookupDataFamInst_maybe fam_inst_envs tc tc_args
  | isDataFamilyTyCon tc
  , match : _ <- lookupFamInstEnv fam_inst_envs tc tc_args
  , FamInstMatch { fim_instance = rep_fam@(FamInst { fi_axiom = ax
                                                   , fi_cvs   = cvs })
                 , fim_tys      = rep_args
                 , fim_cos      = rep_cos } <- match
  , let rep_tc = dataFamInstRepTyCon rep_fam
        co     = mkUnbranchedAxInstCo Representational ax rep_args
                                      (mkCoVarCos cvs)
  = ASSERT( null rep_cos ) -- See Note [Constrained family instances] in FamInstEnv
    Just (rep_tc, rep_args, co)

  | otherwise
  = Nothing

-- | 'tcTopNormaliseNewTypeTF_maybe' gets rid of top-level newtypes,
-- potentially looking through newtype /instances/.
--
-- It is only used by the type inference engine (specifically, when
-- solving representational equality), and hence it is careful to unwrap
-- only if the relevant data constructor is in scope.  That's why
-- it get a GlobalRdrEnv argument.
--
-- It is careful not to unwrap data/newtype instances if it can't
-- continue unwrapping.  Such care is necessary for proper error
-- messages.
--
-- It does not look through type families.
-- It does not normalise arguments to a tycon.
--
-- If the result is Just (rep_ty, (co, gres), rep_ty), then
--    co : ty ~R rep_ty
--    gres are the GREs for the data constructors that
--                          had to be in scope
tcTopNormaliseNewTypeTF_maybe :: FamInstEnvs
                              -> GlobalRdrEnv
                              -> Type
                              -> Maybe ((Bag GlobalRdrElt, TcCoercion), Type)
tcTopNormaliseNewTypeTF_maybe faminsts rdr_env ty
-- cf. FamInstEnv.topNormaliseType_maybe and Coercion.topNormaliseNewType_maybe
  = topNormaliseTypeX stepper plus ty
  where
    plus :: (Bag GlobalRdrElt, TcCoercion) -> (Bag GlobalRdrElt, TcCoercion)
         -> (Bag GlobalRdrElt, TcCoercion)
    plus (gres1, co1) (gres2, co2) = ( gres1 `unionBags` gres2
                                     , co1 `mkTransCo` co2 )

    stepper :: NormaliseStepper (Bag GlobalRdrElt, TcCoercion)
    stepper = unwrap_newtype `composeSteppers` unwrap_newtype_instance

    -- For newtype instances we take a double step or nothing, so that
    -- we don't return the representation type of the newtype instance,
    -- which would lead to terrible error messages
    unwrap_newtype_instance rec_nts tc tys
      | Just (tc', tys', co) <- tcLookupDataFamInst_maybe faminsts tc tys
      = mapStepResult (\(gres, co1) -> (gres, co `mkTransCo` co1)) $
        unwrap_newtype rec_nts tc' tys'
      | otherwise = NS_Done

    unwrap_newtype rec_nts tc tys
      | Just con <- newTyConDataCon_maybe tc
      , Just gre <- lookupGRE_Name rdr_env (dataConName con)
           -- This is where we check that the
           -- data constructor is in scope
      = mapStepResult (\co -> (unitBag gre, co)) $
        unwrapNewTypeStepper rec_nts tc tys

      | otherwise
      = NS_Done

{-
************************************************************************
*                                                                      *
        Extending the family instance environment
*                                                                      *
************************************************************************
-}

-- Add new locally-defined family instances
tcExtendLocalFamInstEnv :: [FamInst] -> TcM a -> TcM a
tcExtendLocalFamInstEnv fam_insts thing_inside
 = do { env <- getGblEnv
      ; (inst_env', fam_insts') <- foldlM addLocalFamInst
                                       (tcg_fam_inst_env env, tcg_fam_insts env)
                                       fam_insts
      ; let env' = env { tcg_fam_insts    = fam_insts'
                       , tcg_fam_inst_env = inst_env' }
      ; setGblEnv env' thing_inside
      }

-- Check that the proposed new instance is OK,
-- and then add it to the home inst env
-- This must be lazy in the fam_inst arguments, see Note [Lazy axiom match]
-- in FamInstEnv.hs
addLocalFamInst :: (FamInstEnv,[FamInst])
                -> FamInst
                -> TcM (FamInstEnv, [FamInst])
addLocalFamInst (home_fie, my_fis) fam_inst
        -- home_fie includes home package and this module
        -- my_fies is just the ones from this module
  = do { traceTc "addLocalFamInst" (ppr fam_inst)

       ; isGHCi <- getIsGHCi
       ; mod <- getModule
       ; traceTc "alfi" (ppr mod $$ ppr isGHCi)

           -- In GHCi, we *override* any identical instances
           -- that are also defined in the interactive context
           -- See Note [Override identical instances in GHCi] in HscTypes
       ; let home_fie'
               | isGHCi    = deleteFromFamInstEnv home_fie fam_inst
               | otherwise = home_fie

           -- Load imported instances, so that we report
           -- overlaps correctly
       ; eps <- getEps
       ; let inst_envs  = (eps_fam_inst_env eps, home_fie')
             home_fie'' = extendFamInstEnv home_fie fam_inst

           -- Check for conflicting instance decls and injectivity violations
       ; no_conflict    <- checkForConflicts            inst_envs fam_inst
       ; injectivity_ok <- checkForInjectivityConflicts inst_envs fam_inst

       ; if no_conflict && injectivity_ok then
            return (home_fie'', fam_inst : my_fis)
         else
            return (home_fie,   my_fis) }

{-
************************************************************************
*                                                                      *
        Checking an instance against conflicts with an instance env
*                                                                      *
************************************************************************

Check whether a single family instance conflicts with those in two instance
environments (one for the EPS and one for the HPT).
-}

checkForConflicts :: FamInstEnvs -> FamInst -> TcM Bool
checkForConflicts inst_envs fam_inst
  = do { let conflicts = lookupFamInstEnvConflicts inst_envs fam_inst
             no_conflicts = null conflicts
       ; traceTc "checkForConflicts" $
         vcat [ ppr (map fim_instance conflicts)
              , ppr fam_inst
              -- , ppr inst_envs
         ]
       ; unless no_conflicts $ conflictInstErr fam_inst conflicts
       ; return no_conflicts }

-- | Check whether a new open type family equation can be added without
-- violating injectivity annotation supplied by the user. Returns True when
-- this is possible and False if adding this equation would violate injectivity
-- annotation.
checkForInjectivityConflicts :: FamInstEnvs -> FamInst -> TcM Bool
checkForInjectivityConflicts instEnvs famInst
    | isTypeFamilyTyCon tycon
    -- type family is injective in at least one argument
    , Injective inj <- familyTyConInjectivityInfo tycon = do
    { let axiom = coAxiomSingleBranch fi_ax
          conflicts = lookupFamInstEnvInjectivityConflicts inj instEnvs famInst
          -- see Note [Verifying injectivity annotation] in FamInstEnv
          errs = makeInjectivityErrors fi_ax axiom inj conflicts
    ; mapM_ (\(err, span) -> setSrcSpan span $ addErr err) errs
    ; return (null errs)
    }

    -- if there was no injectivity annotation or tycon does not represent a
    -- type family we report no conflicts
    | otherwise = return True
    where tycon = famInstTyCon famInst
          fi_ax = fi_axiom famInst

-- | Build a list of injectivity errors together with their source locations.
makeInjectivityErrors
   :: CoAxiom br   -- ^ Type family for which we generate errors
   -> CoAxBranch   -- ^ Currently checked equation (represented by axiom)
   -> [Bool]       -- ^ Injectivity annotation
   -> [CoAxBranch] -- ^ List of injectivity conflicts
   -> [(SDoc, SrcSpan)]
makeInjectivityErrors fi_ax axiom inj conflicts
  = ASSERT2( any id inj, text "No injective type variables" )
    let lhs             = coAxBranchLHS axiom
        rhs             = coAxBranchRHS axiom

        are_conflicts   = not $ null conflicts
        unused_inj_tvs  = unusedInjTvsInRHS (coAxiomTyCon fi_ax) inj lhs rhs
        inj_tvs_unused  = not $ and (isEmptyVarSet <$> unused_inj_tvs)
        tf_headed       = isTFHeaded rhs
        bare_variables  = bareTvInRHSViolated lhs rhs
        wrong_bare_rhs  = not $ null bare_variables

        err_builder herald eqns
                        = ( hang herald
                               2 (vcat (map (pprCoAxBranch fi_ax) eqns))
                          , coAxBranchSpan (head eqns) )
        errorIf p f     = if p then [f err_builder axiom] else []
     in    errorIf are_conflicts  (conflictInjInstErr     conflicts     )
        ++ errorIf inj_tvs_unused (unusedInjectiveVarsErr unused_inj_tvs)
        ++ errorIf tf_headed       tfHeadedErr
        ++ errorIf wrong_bare_rhs (bareVariableInRHSErr   bare_variables)


-- | Return a list of type variables that the function is injective in and that
-- do not appear on injective positions in the RHS of a family instance
-- declaration. The returned Pair includes invisible vars followed by visible ones
unusedInjTvsInRHS :: TyCon -> [Bool] -> [Type] -> Type -> Pair TyVarSet
-- INVARIANT: [Bool] list contains at least one True value
-- See Note [Verifying injectivity annotation]. This function implements fourth
-- check described there.
-- In theory, instead of implementing this whole check in this way, we could
-- attempt to unify equation with itself.  We would reject exactly the same
-- equations but this method gives us more precise error messages by returning
-- precise names of variables that are not mentioned in the RHS.
unusedInjTvsInRHS tycon injList lhs rhs =
  (`minusVarSet` injRhsVars) <$> injLHSVars
    where
      -- set of type and kind variables in which type family is injective
      (invis_pairs, vis_pairs)
        = partitionInvisibles tycon snd (zipEqual "unusedInjTvsInRHS" injList lhs)
      invis_lhs = uncurry filterByList $ unzip invis_pairs
      vis_lhs   = uncurry filterByList $ unzip vis_pairs

      invis_vars = tyCoVarsOfTypes invis_lhs
      Pair invis_vars' vis_vars = splitVisVarsOfTypes vis_lhs
      injLHSVars
        = Pair (invis_vars `minusVarSet` vis_vars `unionVarSet` invis_vars')
               vis_vars

      -- set of type variables appearing in the RHS on an injective position.
      -- For all returned variables we assume their associated kind variables
      -- also appear in the RHS.
      injRhsVars = collectInjVars rhs

      -- Collect all type variables that are either arguments to a type
      -- constructor or to injective type families.
      collectInjVars :: Type -> VarSet
      collectInjVars (TyVarTy v)
        = unitVarSet v `unionVarSet` collectInjVars (tyVarKind v)
      collectInjVars (TyConApp tc tys)
        | isTypeFamilyTyCon tc = collectInjTFVars tys
                                                 (familyTyConInjectivityInfo tc)
        | otherwise            = mapUnionVarSet collectInjVars tys
      collectInjVars (LitTy {})
        = emptyVarSet
      collectInjVars (FunTy arg res)
        = collectInjVars arg `unionVarSet` collectInjVars res
      collectInjVars (AppTy fun arg)
        = collectInjVars fun `unionVarSet` collectInjVars arg
      -- no forall types in the RHS of a type family
      collectInjVars (ForAllTy {})    =
          panic "unusedInjTvsInRHS.collectInjVars"
      collectInjVars (CastTy ty _)   = collectInjVars ty
      collectInjVars (CoercionTy {}) = emptyVarSet

      collectInjTFVars :: [Type] -> Injectivity -> VarSet
      collectInjTFVars _ NotInjective
          = emptyVarSet
      collectInjTFVars tys (Injective injList)
          = mapUnionVarSet collectInjVars (filterByList injList tys)


-- | Is type headed by a type family application?
isTFHeaded :: Type -> Bool
-- See Note [Verifying injectivity annotation]. This function implements third
-- check described there.
isTFHeaded ty | Just ty' <- coreView ty
              = isTFHeaded ty'
isTFHeaded ty | (TyConApp tc args) <- ty
              , isTypeFamilyTyCon tc
              = tyConArity tc == length args
isTFHeaded _  = False


-- | If a RHS is a bare type variable return a set of LHS patterns that are not
-- bare type variables.
bareTvInRHSViolated :: [Type] -> Type -> [Type]
-- See Note [Verifying injectivity annotation]. This function implements second
-- check described there.
bareTvInRHSViolated pats rhs | isTyVarTy rhs
   = filter (not . isTyVarTy) pats
bareTvInRHSViolated _ _ = []


conflictInstErr :: FamInst -> [FamInstMatch] -> TcRn ()
conflictInstErr fam_inst conflictingMatch
  | (FamInstMatch { fim_instance = confInst }) : _ <- conflictingMatch
  = let (err, span) = makeFamInstsErr
                            (text "Conflicting family instance declarations:")
                            [fam_inst, confInst]
    in setSrcSpan span $ addErr err
  | otherwise
  = panic "conflictInstErr"

-- | Type of functions that use error message and a list of axioms to build full
-- error message (with a source location) for injective type families.
type InjErrorBuilder = SDoc -> [CoAxBranch] -> (SDoc, SrcSpan)

-- | Build injecivity error herald common to all injectivity errors.
injectivityErrorHerald :: Bool -> SDoc
injectivityErrorHerald isSingular =
  text "Type family equation" <> s isSingular <+> text "violate" <>
  s (not isSingular) <+> text "injectivity annotation" <>
  if isSingular then dot else colon
  -- Above is an ugly hack.  We want this: "sentence. herald:" (note the dot and
  -- colon).  But if herald is empty we want "sentence:" (note the colon).  We
  -- can't test herald for emptiness so we rely on the fact that herald is empty
  -- only when isSingular is False.  If herald is non empty it must end with a
  -- colon.
    where
      s False = text "s"
      s True  = empty

-- | Build error message for a pair of equations violating an injectivity
-- annotation.
conflictInjInstErr :: [CoAxBranch] -> InjErrorBuilder -> CoAxBranch
                   -> (SDoc, SrcSpan)
conflictInjInstErr conflictingEqns errorBuilder tyfamEqn
  | confEqn : _ <- conflictingEqns
  = errorBuilder (injectivityErrorHerald False) [confEqn, tyfamEqn]
  | otherwise
  = panic "conflictInjInstErr"

-- | Build error message for equation with injective type variables unused in
-- the RHS.
unusedInjectiveVarsErr :: Pair TyVarSet -> InjErrorBuilder -> CoAxBranch
                       -> (SDoc, SrcSpan)
unusedInjectiveVarsErr (Pair invis_vars vis_vars) errorBuilder tyfamEqn
  = errorBuilder (injectivityErrorHerald True $$ msg)
                 [tyfamEqn]
    where
      tvs = invis_vars `unionVarSet` vis_vars
      has_types = not $ isEmptyVarSet vis_vars
      has_kinds = not $ isEmptyVarSet invis_vars

      doc = sep [ what <+> text "variable" <>
                  pluralVarSet tvs <+> pprVarSet tvs (pprQuotedList . toposortTyVars)
                , text "cannot be inferred from the right-hand side." ]
      what = case (has_types, has_kinds) of
               (True, True)   -> text "Type and kind"
               (True, False)  -> text "Type"
               (False, True)  -> text "Kind"
               (False, False) -> pprPanic "mkUnusedInjectiveVarsErr" $ ppr tvs
      print_kinds_info = ppWhen has_kinds ppSuggestExplicitKinds
      msg = doc $$ print_kinds_info $$
            text "In the type family equation:"

-- | Build error message for equation that has a type family call at the top
-- level of RHS
tfHeadedErr :: InjErrorBuilder -> CoAxBranch
            -> (SDoc, SrcSpan)
tfHeadedErr errorBuilder famInst
  = errorBuilder (injectivityErrorHerald True $$
                  text "RHS of injective type family equation cannot" <+>
                  text "be a type family:") [famInst]

-- | Build error message for equation that has a bare type variable in the RHS
-- but LHS pattern is not a bare type variable.
bareVariableInRHSErr :: [Type] -> InjErrorBuilder -> CoAxBranch
                     -> (SDoc, SrcSpan)
bareVariableInRHSErr tys errorBuilder famInst
  = errorBuilder (injectivityErrorHerald True $$
                  text "RHS of injective type family equation is a bare" <+>
                  text "type variable" $$
                  text "but these LHS type and kind patterns are not bare" <+>
                  text "variables:" <+> pprQuotedList tys) [famInst]


makeFamInstsErr :: SDoc -> [FamInst] -> (SDoc, SrcSpan)
makeFamInstsErr herald insts
  = ASSERT( not (null insts) )
    ( hang herald
         2 (vcat [ pprCoAxBranchHdr (famInstAxiom fi) 0
                 | fi <- sorted ])
    , srcSpan )
 where
   getSpan = getSrcLoc . famInstAxiom
   sorted  = sortWith getSpan insts
   fi1     = head sorted
   srcSpan = coAxBranchSpan (coAxiomSingleBranch (famInstAxiom fi1))
   -- The sortWith just arranges that instances are dislayed in order
   -- of source location, which reduced wobbling in error messages,
   -- and is better for users

tcGetFamInstEnvs :: TcM FamInstEnvs
-- Gets both the external-package inst-env
-- and the home-pkg inst env (includes module being compiled)
tcGetFamInstEnvs
  = do { eps <- getEps; env <- getGblEnv
       ; return (eps_fam_inst_env eps, tcg_fam_inst_env env) }
