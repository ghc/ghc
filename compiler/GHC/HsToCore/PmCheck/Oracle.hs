{-
Authors: George Karachalias <george.karachalias@cs.kuleuven.be>
         Sebastian Graf <sgraf1337@gmail.com>
         Ryan Scott <ryan.gl.scott@gmail.com>
-}

{-# LANGUAGE CPP, LambdaCase, TupleSections, PatternSynonyms, ViewPatterns, MultiWayIf, ScopedTypeVariables #-}

-- | The pattern match oracle. The main export of the module are the functions
-- 'addPmCts' for adding facts to the oracle, and 'provideEvidence' to turn a
-- 'Nabla' into a concrete evidence for an equation.
--
-- In terms of the [Lower Your Guards paper](https://dl.acm.org/doi/abs/10.1145/3408989)
-- describing the implementation, this module is concerned with Sections 3.4, 3.6 and 3.7.
-- E.g., it represents refinement types diretly as a normalised refinement type 'Nabla'.
module GHC.HsToCore.PmCheck.Oracle (

        DsM, tracePm, mkPmId,
        Nabla, initNablas, lookupRefuts, lookupSolution,

        PmCt(PmTyCt), PmCts, pattern PmVarCt, pattern PmCoreCt,
        pattern PmConCt, pattern PmNotConCt, pattern PmBotCt,
        pattern PmNotBotCt,

        addPmCts,           -- Add a constraint to the oracle.
        provideEvidence
    ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.HsToCore.PmCheck.Types

import GHC.Driver.Session
import GHC.Driver.Config
import GHC.Utils.Outputable
import GHC.Utils.Error
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Data.Bag
import GHC.Types.Unique.Set
import GHC.Types.Unique.DSet
import GHC.Types.Unique
import GHC.Types.Id
import GHC.Types.Var.Env
import GHC.Types.Unique.DFM
import GHC.Types.Var      (EvVar)
import GHC.Types.Name
import GHC.Core
import GHC.Core.FVs       (exprFreeVars)
import GHC.Core.Map
import GHC.Core.SimpleOpt (simpleOptExpr, exprIsConApp_maybe)
import GHC.Core.Utils     (exprType)
import GHC.Core.Make      (mkListExpr, mkCharExpr)
import GHC.Types.Unique.Supply
import GHC.Data.FastString
import GHC.Types.SrcLoc
import GHC.Data.Maybe
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Core.PatSyn
import GHC.Core.TyCon
import GHC.Builtin.Types
import GHC.Builtin.Types.Prim (tYPETyCon)
import GHC.Core.TyCo.Rep
import GHC.Core.Type
import GHC.Tc.Solver   (tcNormalise, tcCheckSatisfiability)
import GHC.Core.Unify    (tcMatchTy)
import GHC.Core.Coercion
import GHC.Utils.Monad hiding (foldlM)
import GHC.HsToCore.Monad hiding (foldlM)
import GHC.Tc.Instance.Family
import GHC.Core.FamInstEnv

import Control.Applicative ((<|>))
import Control.Monad (guard, mzero, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Data.Bifunctor (second)
import Data.Either   (partitionEithers)
import Data.Foldable (foldlM, minimumBy, toList)
import Data.List     (find)
import Data.Ord      (comparing)
import qualified Data.Semigroup as Semigroup
import Data.Tuple    (swap)

-- Debugging Infrastructure

tracePm :: String -> SDoc -> DsM ()
tracePm herald doc = do
  dflags <- getDynFlags
  printer <- mkPrintUnqualifiedDs
  liftIO $ dumpIfSet_dyn_printer printer dflags
            Opt_D_dump_ec_trace "" FormatText (text herald $$ (nest 2 doc))
{-# INLINE tracePm #-}  -- see Note [INLINE conditional tracing utilities]

-- | Generate a fresh `Id` of a given type
mkPmId :: Type -> DsM Id
mkPmId ty = getUniqueM >>= \unique ->
  let occname = mkVarOccFS $ fsLit "pm"
      name    = mkInternalName unique occname noSrcSpan
  in  return (mkLocalIdOrCoVar name Many ty)

-----------------------------------------------
-- * Caching possible matches of a COMPLETE set

-- See Note [Implementation of COMPLETE pragmas]

-- | Traverse the COMPLETE sets of 'ResidualCompleteMatches'.
trvRcm :: Applicative f => (ConLikeSet -> f ConLikeSet) -> ResidualCompleteMatches -> f ResidualCompleteMatches
trvRcm f (RCM vanilla pragmas) = RCM <$> traverse f vanilla
                                     <*> traverse (traverse f) pragmas
-- | Update the COMPLETE sets of 'ResidualCompleteMatches'.
updRcm :: (ConLikeSet -> ConLikeSet) -> ResidualCompleteMatches -> ResidualCompleteMatches
updRcm f (RCM vanilla pragmas) = RCM (f <$> vanilla) (fmap f <$> pragmas)

-- | A pseudo-'CompleteMatch' for the vanilla complete set of the given data
-- 'TyCon'.
-- Ex.: @vanillaCompleteMatchTC 'Maybe' ==> Just ("Maybe", {'Just','Nothing'})@
vanillaCompleteMatchTC :: TyCon -> Maybe ConLikeSet
vanillaCompleteMatchTC tc =
  let -- | TYPE acts like an empty data type on the term-level (#14086), but
      -- it is a PrimTyCon, so tyConDataCons_maybe returns Nothing. Hence a
      -- special case.
      mb_dcs | tc == tYPETyCon = Just []
             | otherwise       = tyConDataCons_maybe tc
  in mkUniqDSet . map RealDataCon <$> mb_dcs

-- | Initialise from 'dsGetCompleteMatches' (containing all COMPLETE pragmas)
-- if the given 'ResidualCompleteMatches' were empty.
addCompleteMatches :: ResidualCompleteMatches -> DsM ResidualCompleteMatches
addCompleteMatches (RCM v Nothing) = RCM v . Just <$> dsGetCompleteMatches
addCompleteMatches rcm             = pure rcm

-- | Adds the declared 'CompleteMatches' from COMPLETE pragmas, as well as the
-- vanilla data defn if it is a 'DataCon'.
addConLikeMatches :: ConLike -> ResidualCompleteMatches -> DsM ResidualCompleteMatches
addConLikeMatches (RealDataCon dc) rcm = addTyConMatches (dataConTyCon dc) rcm
addConLikeMatches (PatSynCon _)    rcm = addCompleteMatches rcm

-- | Adds
--    * the 'CompleteMatches' from COMPLETE pragmas
--    * and the /vanilla/ 'CompleteMatch' from the data 'TyCon'
-- to the 'ResidualCompleteMatches', if not already present.
addTyConMatches :: TyCon -> ResidualCompleteMatches -> DsM ResidualCompleteMatches
addTyConMatches tc rcm = add_tc_match <$> addCompleteMatches rcm
  where
    -- | Add the vanilla COMPLETE set from the data defn, if any. But only if
    -- it's not already present.
    add_tc_match rcm
      = rcm{rcm_vanilla = rcm_vanilla rcm <|> vanillaCompleteMatchTC tc}

markMatched :: ConLike -> ResidualCompleteMatches -> DsM ResidualCompleteMatches
markMatched cl rcm = do
  rcm' <- addConLikeMatches cl rcm
  pure $ updRcm (flip delOneFromUniqDSet cl) rcm'

{-
Note [Implementation of COMPLETE pragmas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A COMPLETE set represents a set of conlikes (i.e., constructors or
pattern synonyms) such that if they are all pattern-matched against in a
function, it gives rise to a total function. An example is:

  newtype Boolean = Boolean Int
  pattern F, T :: Boolean
  pattern F = Boolean 0
  pattern T = Boolean 1
  {-# COMPLETE F, T #-}

  -- This is a total function
  booleanToInt :: Boolean -> Int
  booleanToInt F = 0
  booleanToInt T = 1

COMPLETE sets are represented internally in GHC a set of 'ConLike's. For
example, the pragma {-# COMPLETE F, T #-} would be represented as:

  {F, T}

GHC collects all COMPLETE pragmas from the current module and from imports
into a field in the DsM environment, which can be accessed with
dsGetCompleteMatches from "GHC.HsToCore.Monad".
Currently, COMPLETE pragmas can't be orphans (e.g. at least one ConLike must
also be defined in the module of the pragma) and do not impact recompilation
checking (#18675).

The pattern-match checker will then initialise each variable's 'VarInfo' with
*all* imported COMPLETE sets (in 'GHC.HsToCore.PmCheck.Oracle.addCompleteMatches'),
well-typed or not, into a 'ResidualCompleteMatches'. The trick is that a
COMPLETE set that is ill-typed for that match variable could never be written by
the user! And we make sure not to report any ill-typed COMPLETE sets when
formatting 'Nabla's for warnings in 'provideEvidence'.

A 'ResidualCompleteMatches' is a list of all COMPLETE sets, minus the ConLikes
we know a particular variable can't be (through negative constructor constraints
@x /~ K@ or a failed attempt at instantiating that ConLike during inhabitation
testing). If *any* of the COMPLETE sets become empty, we know that the match
was exhaustive.

We assume that a COMPLETE set is non-empty if for one of its ConLikes
we fail to 'guessConLikeUnivTyArgsFromResTy'. That accounts for ill-typed
COMPLETE sets. So why don't we simply prune those ill-typed COMPLETE sets from
'ResidualCompleteMatches'? The answer is that additional type constraints might
make more COMPLETE sets applicable! Example:

  f :: a -> a :~: Boolean -> ()
  f x Refl | T <- x = ()
           | F <- x = ()

If we eagerly prune {F,T} from the residual matches of @x@, then we don't see
that the match in the guards of @f@ is exhaustive, where the COMPLETE set
applies due to refined type information.
-}

---------------------------------------------------
-- * Instantiating constructors, types and evidence

-- | Instantiate a 'ConLike' given its universal type arguments. Instantiates
-- existential and term binders with fresh variables of appropriate type.
-- Returns instantiated type and term variables from the match, type evidence
-- and the types of strict constructor fields.
mkOneConFull :: [Type] -> ConLike -> DsM ([TyVar], [Id], Bag TyCt, [Type])
--  * 'con' K is a ConLike
--       - In the case of DataCons and most PatSynCons, these
--         are associated with a particular TyCon T
--       - But there are PatSynCons for this is not the case! See #11336, #17112
--
--  * 'arg_tys' tys are the types K's universally quantified type
--     variables should be instantiated to.
--       - For DataCons and most PatSyns these are the arguments of their TyCon
--       - For cases like the PatSyns in #11336, #17112, we can't easily guess
--         these, so don't call this function.
--
-- After instantiating the universal tyvars of K to tys we get
--          K @tys :: forall bs. Q => s1 .. sn -> T tys
-- Note that if K is a PatSynCon, depending on arg_tys, T might not necessarily
-- be a concrete TyCon.
--
-- Suppose y1 is a strict field. Then we get
-- Results: bs
--          [y1,..,yn]
--          Q
--          [s1]
mkOneConFull arg_tys con = do
  let (univ_tvs, ex_tvs, eq_spec, thetas, _req_theta, field_tys, _con_res_ty)
        = conLikeFullSig con
  -- pprTrace "mkOneConFull" (ppr con $$ ppr arg_tys $$ ppr univ_tvs $$ ppr _con_res_ty) (return ())
  -- Substitute universals for type arguments
  let subst_univ = zipTvSubst univ_tvs arg_tys
  -- Instantiate fresh existentials as arguments to the constructor. This is
  -- important for instantiating the Thetas and field types.
  (subst, _) <- cloneTyVarBndrs subst_univ ex_tvs <$> getUniqueSupplyM
  let field_tys' = substTys subst $ map scaledThing field_tys
  -- Instantiate fresh term variables (VAs) as arguments to the constructor
  vars <- mapM mkPmId field_tys'
  -- All constraints bound by the constructor (alpha-renamed), these are added
  -- to the type oracle
  let ty_cs = substTheta subst (eqSpecPreds eq_spec ++ thetas)
  -- Figure out the types of strict constructor fields
  let arg_is_strict = map isBanged $ conLikeImplBangs con
      strict_arg_tys = filterByList arg_is_strict field_tys'
  return (ex_tvs, vars, listToBag ty_cs, strict_arg_tys)

-------------------------
-- * Pattern match oracle


-------------------------------------
-- * Composable satisfiability checks

-- | Given a 'Nabla', check if it is compatible with new facts encoded in this
-- this check. If so, return 'Just' a potentially extended 'Nabla'. Return
-- 'Nothing' if unsatisfiable.
--
-- There are three essential SatisfiabilityChecks:
--   1. 'tmIsSatisfiable', adding term oracle facts
--   2. 'tyIsSatisfiable', adding type oracle facts
--   3. 'tysAreNonVoid', checks if the given types have an inhabitant
-- Functions like 'pmIsSatisfiable', 'nonVoid' and 'testInhabited' plug these
-- together as they see fit.
newtype SatisfiabilityCheck = SC (Nabla -> DsM (Maybe Nabla))

-- | Check the given 'Nabla' for satisfiability by the given
-- 'SatisfiabilityCheck'. Return 'Just' a new, potentially extended, 'Nabla' if
-- successful, and 'Nothing' otherwise.
runSatisfiabilityCheck :: Nabla -> SatisfiabilityCheck -> DsM (Maybe Nabla)
runSatisfiabilityCheck nabla (SC chk) = chk nabla

-- | Allowing easy composition of 'SatisfiabilityCheck's.
instance Semigroup SatisfiabilityCheck where
  -- This is @a >=> b@ from MaybeT DsM
  SC a <> SC b = SC c
    where
      c nabla = a nabla >>= \case
        Nothing     -> pure Nothing
        Just nabla' -> b nabla'

instance Monoid SatisfiabilityCheck where
  -- We only need this because of mconcat (which we use in place of sconcat,
  -- which requires NonEmpty lists as argument, making all call sites ugly)
  mempty = SC (pure . Just)

-------------------------------
-- * Oracle transition function

-- | Given a conlike's term constraints, type constraints, and strict argument
-- types, check if they are satisfiable.
-- (In other words, this is the ⊢_Sat oracle judgment from the GADTs Meet
-- Their Match paper.)
--
-- Taking strict argument types into account is something which was not
-- discussed in GADTs Meet Their Match. For an explanation of what role they
-- serve, see @Note [Strict argument type constraints]@.
pmIsSatisfiable
  :: Nabla       -- ^ The ambient term and type constraints
                 --   (known to be satisfiable).
  -> Bag TyCt    -- ^ The new type constraints.
  -> Bag TmCt    -- ^ The new term constraints.
  -> [Type]      -- ^ The strict argument types.
  -> DsM (Maybe Nabla)
                 -- ^ @'Just' nabla@ if the constraints (@nabla@) are
                 -- satisfiable, and each strict argument type is inhabitable.
                 -- 'Nothing' otherwise.
pmIsSatisfiable amb_cs new_ty_cs new_tm_cs strict_arg_tys =
  -- The order is important here! Check the new type constraints before we check
  -- whether strict argument types are inhabited given those constraints.
  runSatisfiabilityCheck amb_cs $ mconcat
    [ tyIsSatisfiable True new_ty_cs
    , tmIsSatisfiable new_tm_cs
    , tysAreNonVoid initRecTc strict_arg_tys
    ]

-----------------------
-- * Type normalisation

-- | The return value of 'pmTopNormaliseType'
data TopNormaliseTypeResult
  = NoChange Type
  -- ^ 'tcNormalise' failed to simplify the type and 'topNormaliseTypeX' was
  -- unable to reduce the outermost type application, so the type came out
  -- unchanged.
  | NormalisedByConstraints Type
  -- ^ 'tcNormalise' was able to simplify the type with some local constraint
  -- from the type oracle, but 'topNormaliseTypeX' couldn't identify a type
  -- redex.
  | HadRedexes Type [(Type, DataCon, Type)] Type
  -- ^ 'tcNormalise' may or may not been able to simplify the type, but
  -- 'topNormaliseTypeX' made progress either way and got rid of at least one
  -- outermost type or data family redex or newtype.
  -- The first field is the last type that was reduced solely through type
  -- family applications (possibly just the 'tcNormalise'd type). This is the
  -- one that is equal (in source Haskell) to the initial type.
  -- The third field is the type that we get when also looking through data
  -- family applications and newtypes. This would be the representation type in
  -- Core (modulo casts).
  -- The second field is the list of Newtype 'DataCon's that we looked through
  -- in the chain of reduction steps between the Source type and the Core type.
  -- We also keep the type of the DataCon application and its field, so that we
  -- don't have to reconstruct it in 'inhabitationCandidates' and
  -- 'provideEvidence'.
  -- For an example, see Note [Type normalisation].

-- | Just give me the potentially normalised source type, unchanged or not!
normalisedSourceType :: TopNormaliseTypeResult -> Type
normalisedSourceType (NoChange ty)                = ty
normalisedSourceType (NormalisedByConstraints ty) = ty
normalisedSourceType (HadRedexes ty _ _)          = ty

-- | Return the fields of 'HadRedexes'. Returns appropriate defaults in the
-- other cases.
tntrGuts :: TopNormaliseTypeResult -> (Type, [(Type, DataCon, Type)], Type)
tntrGuts (NoChange ty)                  = (ty,     [],      ty)
tntrGuts (NormalisedByConstraints ty)   = (ty,     [],      ty)
tntrGuts (HadRedexes src_ty ds core_ty) = (src_ty, ds, core_ty)

instance Outputable TopNormaliseTypeResult where
  ppr (NoChange ty)                  = text "NoChange" <+> ppr ty
  ppr (NormalisedByConstraints ty)   = text "NormalisedByConstraints" <+> ppr ty
  ppr (HadRedexes src_ty ds core_ty) = text "HadRedexes" <+> braces fields
    where
      fields = fsep (punctuate comma [ text "src_ty =" <+> ppr src_ty
                                     , text "newtype_dcs =" <+> ppr ds
                                     , text "core_ty =" <+> ppr core_ty ])

pmTopNormaliseType :: TyState -> Type -> DsM TopNormaliseTypeResult
-- ^ Get rid of *outermost* (or toplevel)
--      * type function redex
--      * data family redex
--      * newtypes
--
-- Behaves like `topNormaliseType_maybe`, but instead of returning a
-- coercion, it returns useful information for issuing pattern matching
-- warnings. See Note [Type normalisation] for details.
-- It also initially 'tcNormalise's the type with the bag of local constraints.
--
-- See 'TopNormaliseTypeResult' for the meaning of the return value.
--
-- NB: Normalisation can potentially change kinds, if the head of the type
-- is a type family with a variable result kind. I (Richard E) can't think
-- of a way to cause trouble here, though.
pmTopNormaliseType (TySt inert) typ
  = do env <- dsGetFamInstEnvs
       -- Before proceeding, we chuck typ into the constraint solver, in case
       -- solving for given equalities may reduce typ some. See
       -- "Wrinkle: local equalities" in Note [Type normalisation].
       (_, mb_typ') <- initTcDsForSolver $ tcNormalise inert typ
       -- If tcNormalise didn't manage to simplify the type, continue anyway.
       -- We might be able to reduce type applications nonetheless!
       let typ' = fromMaybe typ mb_typ'
       -- Now we look with topNormaliseTypeX through type and data family
       -- applications and newtypes, which tcNormalise does not do.
       -- See also 'TopNormaliseTypeResult'.
       pure $ case topNormaliseTypeX (stepper env) comb typ' of
         Nothing
           | Nothing <- mb_typ' -> NoChange typ
           | otherwise          -> NormalisedByConstraints typ'
         Just ((ty_f,tm_f), ty) -> HadRedexes src_ty newtype_dcs core_ty
           where
             src_ty = eq_src_ty ty (typ' : ty_f [ty])
             newtype_dcs = tm_f []
             core_ty = ty
  where
    -- Find the first type in the sequence of rewrites that is a data type,
    -- newtype, or a data family application (not the representation tycon!).
    -- This is the one that is equal (in source Haskell) to the initial type.
    -- If none is found in the list, then all of them are type family
    -- applications, so we simply return the last one, which is the *simplest*.
    eq_src_ty :: Type -> [Type] -> Type
    eq_src_ty ty tys = maybe ty id (find is_closed_or_data_family tys)

    is_closed_or_data_family :: Type -> Bool
    is_closed_or_data_family ty = pmIsClosedType ty || isDataFamilyAppType ty

    -- For efficiency, represent both lists as difference lists.
    -- comb performs the concatenation, for both lists.
    comb (tyf1, tmf1) (tyf2, tmf2) = (tyf1 . tyf2, tmf1 . tmf2)

    stepper env = newTypeStepper `composeSteppers` tyFamStepper env

    -- A 'NormaliseStepper' that unwraps newtypes, careful not to fall into
    -- a loop. If it would fall into a loop, it produces 'NS_Abort'.
    newTypeStepper :: NormaliseStepper ([Type] -> [Type],[(Type, DataCon, Type)] -> [(Type, DataCon, Type)])
    newTypeStepper rec_nts tc tys
      | Just (ty', _co) <- instNewTyCon_maybe tc tys
      , let orig_ty = TyConApp tc tys
      = case checkRecTc rec_nts tc of
          Just rec_nts' -> let tyf = (orig_ty:)
                               tmf = ((orig_ty, tyConSingleDataCon tc, ty'):)
                           in  NS_Step rec_nts' ty' (tyf, tmf)
          Nothing       -> NS_Abort
      | otherwise
      = NS_Done

    tyFamStepper :: FamInstEnvs -> NormaliseStepper ([Type] -> [Type], a -> a)
    tyFamStepper env rec_nts tc tys  -- Try to step a type/data family
      = case topReduceTyFamApp_maybe env tc tys of
          Just (_, rhs, _) -> NS_Step rec_nts rhs ((rhs:), id)
          _                -> NS_Done

-- | Returns 'True' if the argument 'Type' is a fully saturated application of
-- a closed type constructor.
--
-- Closed type constructors are those with a fixed right hand side, as
-- opposed to e.g. associated types. These are of particular interest for
-- pattern-match coverage checking, because GHC can exhaustively consider all
-- possible forms that values of a closed type can take on.
--
-- Note that this function is intended to be used to check types of value-level
-- patterns, so as a consequence, the 'Type' supplied as an argument to this
-- function should be of kind @Type@.
pmIsClosedType :: Type -> Bool
pmIsClosedType ty
  = case splitTyConApp_maybe ty of
      Just (tc, ty_args)
             | is_algebraic_like tc && not (isFamilyTyCon tc)
             -> ASSERT2( ty_args `lengthIs` tyConArity tc, ppr ty ) True
      _other -> False
  where
    -- This returns True for TyCons which /act like/ algebraic types.
    -- (See "Type#type_classification" for what an algebraic type is.)
    --
    -- This is qualified with \"like\" because of a particular special
    -- case: TYPE (the underlyind kind behind Type, among others). TYPE
    -- is conceptually a datatype (and thus algebraic), but in practice it is
    -- a primitive builtin type, so we must check for it specially.
    --
    -- NB: it makes sense to think of TYPE as a closed type in a value-level,
    -- pattern-matching context. However, at the kind level, TYPE is certainly
    -- not closed! Since this function is specifically tailored towards pattern
    -- matching, however, it's OK to label TYPE as closed.
    is_algebraic_like :: TyCon -> Bool
    is_algebraic_like tc = isAlgTyCon tc || tc == tYPETyCon

{- Note [Type normalisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Constructs like -XEmptyCase or a previous unsuccessful pattern match on a data
constructor place a non-void constraint on the matched thing. This means that it
boils down to checking whether the type of the scrutinee is inhabited. Function
pmTopNormaliseType gets rid of the outermost type function/data family redex and
newtypes, in search of an algebraic type constructor, which is easier to check
for inhabitation.

It returns 3 results instead of one, because there are 2 subtle points:
1. Newtypes are isomorphic to the underlying type in core but not in the source
   language,
2. The representational data family tycon is used internally but should not be
   shown to the user

Hence, if pmTopNormaliseType env ty_cs ty = Just (src_ty, dcs, core_ty),
then
  (a) src_ty is the rewritten type which we can show to the user. That is, the
      type we get if we rewrite type families but not data families or
      newtypes.
  (b) dcs is the list of newtype constructors "skipped", every time we normalise
      a newtype to its core representation, we keep track of the source data
      constructor. For convenience, we also track the type we unwrap and the
      type of its field. Example: @Down 42@ => @[(Down @Int, Down, Int)]
  (c) core_ty is the rewritten type. That is,
        pmTopNormaliseType env ty_cs ty = Just (src_ty, dcs, core_ty)
      implies
        topNormaliseType_maybe env ty = Just (co, core_ty)
      for some coercion co.

To see how all cases come into play, consider the following example:

  data family T a :: *
  data instance T Int = T1 | T2 Bool
  -- Which gives rise to FC:
  --   data T a
  --   data R:TInt = T1 | T2 Bool
  --   axiom ax_ti : T Int ~R R:TInt

  newtype G1 = MkG1 (T Int)
  newtype G2 = MkG2 G1

  type instance F Int  = F Char
  type instance F Char = G2

In this case pmTopNormaliseType env ty_cs (F Int) results in

  Just (G2, [(G2,MkG2,G1),(G1,MkG1,T Int)], R:TInt)

Which means that in source Haskell:
  - G2 is equivalent to F Int (in contrast, G1 isn't).
  - if (x : R:TInt) then (MkG2 (MkG1 x) : F Int).

-----
-- Wrinkle: Local equalities
-----

Given the following type family:

  type family F a
  type instance F Int = Void

Should the following program (from #14813) be considered exhaustive?

  f :: (i ~ Int) => F i -> a
  f x = case x of {}

You might think "of course, since `x` is obviously of type Void". But the
idType of `x` is technically F i, not Void, so if we pass F i to
inhabitationCandidates, we'll mistakenly conclude that `f` is non-exhaustive.
In order to avoid this pitfall, we need to normalise the type passed to
pmTopNormaliseType, using the constraint solver to solve for any local
equalities (such as i ~ Int) that may be in scope.
-}

----------------
-- * Type oracle

-- | Allocates a fresh 'EvVar' name for 'PredTy's.
nameTyCt :: PredType -> DsM EvVar
nameTyCt pred_ty = do
  unique <- getUniqueM
  let occname = mkVarOccFS (fsLit ("pm_"++show unique))
      idname  = mkInternalName unique occname noSrcSpan
  return (mkLocalIdOrCoVar idname Many pred_ty)

-- | Add some extra type constraints to the 'TyState'; return 'Nothing' if we
-- find a contradiction (e.g. @Int ~ Bool@).
tyOracle :: TyState -> Bag PredType -> DsM (Maybe TyState)
tyOracle (TySt inert) cts
  = do { evs <- traverse nameTyCt cts
       ; let new_inert = inert `unionBags` evs
       ; tracePm "tyOracle" (ppr cts)
       ; ((_warns, errs), res) <- initTcDsForSolver $ tcCheckSatisfiability new_inert
       ; case res of
            Just True  -> return (Just (TySt new_inert))
            Just False -> return Nothing
            Nothing    -> pprPanic "tyOracle" (vcat $ pprErrMsgBagWithLoc errs) }

-- | A 'SatisfiabilityCheck' based on new type-level constraints.
-- Returns a new 'Nabla' if the new constraints are compatible with existing
-- ones. Doesn't bother calling out to the type oracle if the bag of new type
-- constraints was empty. Will only recheck 'ResidualCompleteMatches' in the term oracle
-- for emptiness if the first argument is 'True'.
tyIsSatisfiable :: Bool -> Bag PredType -> SatisfiabilityCheck
tyIsSatisfiable recheck_complete_sets new_ty_cs = SC $ \nabla ->
  if isEmptyBag new_ty_cs
    then pure (Just nabla)
    else tyOracle (nabla_ty_st nabla) new_ty_cs >>= \case
      Nothing                   -> pure Nothing
      Just ty_st'               -> do
        let nabla' = nabla{ nabla_ty_st = ty_st' }
        if recheck_complete_sets
          then ensureAllInhabited nabla'
          else pure (Just nabla')


{- *********************************************************************
*                                                                      *
              DIdEnv with sharing
*                                                                      *
********************************************************************* -}


{- *********************************************************************
*                                                                      *
                 TmState
          What we know about terms
*                                                                      *
********************************************************************* -}

{- Note [The Pos/Neg invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Invariant applying to each VarInfo: Whenever we have @(C, [y,z])@ in 'vi_pos',
any entry in 'vi_neg' must be incomparable to C (return Nothing) according to
'eqPmAltCons'. Those entries that are comparable either lead to a refutation
or are redundant. Examples:
* @x ~ Just y@, @x /~ [Just]@. 'eqPmAltCon' returns @Equal@, so refute.
* @x ~ Nothing@, @x /~ [Just]@. 'eqPmAltCon' returns @Disjoint@, so negative
  info is redundant and should be discarded.
* @x ~ I# y@, @x /~ [4,2]@. 'eqPmAltCon' returns @PossiblyOverlap@, so orthogal.
  We keep this info in order to be able to refute a redundant match on i.e. 4
  later on.

This carries over to pattern synonyms and overloaded literals. Say, we have
    pattern Just42 = Just 42
    case Just42 of x
      Nothing -> ()
      Just _  -> ()
Even though we had a solution for the value abstraction called x here in form
of a PatSynCon (Just42,[]), this solution is incomparable to both Nothing and
Just. Hence we retain the info in vi_neg, which eventually allows us to detect
the complete pattern match.

The Pos/Neg invariant extends to vi_rcm, which stores essentially positive
information. We make sure that vi_neg and vi_rcm never overlap. This isn't
strictly necessary since vi_rcm is just a cache, so doesn't need to be
accurate: Every suggestion of a possible ConLike from vi_rcm might be
refutable by the type oracle anyway. But it helps to maintain sanity while
debugging traces.

Note [Why record both positive and negative info?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
You might think that knowing positive info (like x ~ Just y) would render
negative info irrelevant, but not so because of pattern synonyms.  E.g we might
know that x cannot match (Foo 4), where pattern Foo p = Just p

Also overloaded literals themselves behave like pattern synonyms. E.g if
postively we know that (x ~ I# y), we might also negatively want to record that
x does not match 45 f 45       = e2 f (I# 22#) = e3 f 45       = e4  --
Overlapped

Note [TmState invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~
The term oracle state is never obviously (i.e., without consulting the type
oracle) contradictory. This implies a few invariants:
* Whenever vi_pos overlaps with vi_neg according to 'eqPmAltCon', we refute.
  This is implied by the Note [Pos/Neg invariant].
* Whenever vi_neg subsumes a COMPLETE set, we refute. We consult vi_rcm to
  detect this, but we could just compare whole COMPLETE sets to vi_neg every
  time, if it weren't for performance.

Maintaining these invariants in 'addVarCt' (the core of the term oracle) and
'addNotConCt' is subtle.
* Merging VarInfos. Example: Add the fact @x ~ y@ (see 'equate').
  - (COMPLETE) If we had @x /~ True@ and @y /~ False@, then we get
    @x /~ [True,False]@. This is vacuous by matter of comparing to the built-in
    COMPLETE set, so should refute.
  - (Pos/Neg) If we had @x /~ True@ and @y ~ True@, we have to refute.
* Adding positive information. Example: Add the fact @x ~ K ys@ (see 'addConCt')
  - (Neg) If we had @x /~ K@, refute.
  - (Pos) If we had @x ~ K2@, and that contradicts the new solution according to
    'eqPmAltCon' (ex. K2 is [] and K is (:)), then refute.
  - (Refine) If we had @x /~ K zs@, unify each y with each z in turn.
* Adding negative information. Example: Add the fact @x /~ Nothing@ (see 'addNotConCt')
  - (Refut) If we have @x ~ K ys@, refute.
  - (COMPLETE) If K=Nothing and we had @x /~ Just@, then we get
    @x /~ [Just,Nothing]@. This is vacuous by matter of comparing to the built-in
    COMPLETE set, so should refute.

Note that merging VarInfo in equate can be done by calling out to 'addConCt' and
'addNotConCt' for each of the facts individually.

Note [Representation of Strings in TmState]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Instead of treating regular String literals as a PmLits, we treat it as a list
of characters in the oracle for better overlap reasoning. The following example
shows why:

  f :: String -> ()
  f ('f':_) = ()
  f "foo"   = ()
  f _       = ()

The second case is redundant, and we like to warn about it. Therefore either
the oracle will have to do some smart conversion between the list and literal
representation or treat is as the list it really is at runtime.

The "smart conversion" has the advantage of leveraging the more compact literal
representation wherever possible, but is really nasty to get right with negative
equalities: Just think of how to encode @x /= "foo"@.
The "list" option is far simpler, but incurs some overhead in representation and
warning messages (which can be alleviated by someone with enough dedication).
-}

-- | A 'SatisfiabilityCheck' based on new term-level constraints.
-- Returns a new 'Nabla' if the new constraints are compatible with existing
-- ones.
tmIsSatisfiable :: Bag TmCt -> SatisfiabilityCheck
tmIsSatisfiable new_tm_cs = SC $ \nabla -> runMaybeT $ foldlM addTmCt nabla new_tm_cs

-----------------------
-- * Looking up VarInfo

emptyRCM :: ResidualCompleteMatches
emptyRCM = RCM Nothing Nothing

emptyVarInfo :: Id -> VarInfo
-- We could initialise @bot@ to @Just False@ in case of an unlifted type here,
-- but it's cleaner to let the user of the constraint solver take care of this.
-- After all, there are also strict fields, the unliftedness of which isn't
-- evident in the type. So treating unlifted types here would never be
-- sufficient anyway.
emptyVarInfo x = VI (idType x) [] emptyPmAltConSet MaybeBot emptyRCM

lookupVarInfo :: TmState -> Id -> VarInfo
-- (lookupVarInfo tms x) tells what we know about 'x'
lookupVarInfo (TmSt env _) x = fromMaybe (emptyVarInfo x) (lookupSDIE env x)

-- | Like @lookupVarInfo ts x@, but @lookupVarInfo ts x = (y, vi)@ also looks
-- through newtype constructors. We have @x ~ N1 (... (Nk y))@ such that the
-- returned @y@ doesn't have a positive newtype constructor constraint
-- associated with it (yet). The 'VarInfo' returned is that of @y@'s
-- representative.
--
-- Careful, this means that @idType x@ might be different to @idType y@, even
-- modulo type normalisation!
--
-- See also Note [Coverage checking Newtype matches].
lookupVarInfoNT :: TmState -> Id -> (Id, VarInfo)
lookupVarInfoNT ts x = case lookupVarInfo ts x of
  VI{ vi_pos = as_newtype -> Just y } -> lookupVarInfoNT ts y
  res                                 -> (x, res)
  where
    as_newtype = listToMaybe . mapMaybe go
    go (PmAltConLike (RealDataCon dc), _, [y])
      | isNewDataCon dc = Just y
    go _                = Nothing

------------------------------------------------
-- * Exported utility functions querying 'Nabla'

{- Note [Coverage checking Newtype matches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Newtypes have quite peculiar match semantics compared to ordinary DataCons. In a
pattern-match, they behave like a irrefutable (lazy) match, but for inhabitation
testing purposes (e.g. at construction sites), they behave rather like a DataCon
with a *strict* field, because they don't contribute their own bottom and are
inhabited iff the wrapped type is inhabited.

This distinction becomes apparent in #17248:

  newtype T2 a = T2 a
  g _      True = ()
  g (T2 _) True = ()
  g !_     True = ()

If we treat Newtypes like we treat regular DataCons, we would mark the third
clause as redundant, which clearly is unsound. The solution:
1. 'isPmAltConMatchStrict' returns False for newtypes, indicating that a
   newtype match is lazy.
2. When we find @x ~ T2 y@, transfer all constraints on @x@ (which involve @⊥@)
   to @y@, similar to what 'equate' does, and don't add a @x /~ ⊥@ constraint.
   This way, the third clause will still be marked as inaccessible RHS instead
   of redundant. This is ensured by calling 'lookupVarInfoNT'.
3. Immediately reject when we find @x /~ T2@.
Handling of Newtypes is also described in the Appendix of the Lower Your Guards paper,
where you can find the solution in a perhaps more digestible format.
-}

lookupRefuts :: Uniquable k => Nabla -> k -> [PmAltCon]
-- Unfortunately we need the extra bit of polymorphism and the unfortunate
-- duplication of lookupVarInfo here.
lookupRefuts MkNabla{ nabla_tm_st = ts@(TmSt (SDIE env) _) } k =
  case lookupUDFM_Directly env (getUnique k) of
    Nothing -> []
    Just (Indirect y) -> pmAltConSetElems (vi_neg (lookupVarInfo ts y))
    Just (Entry vi)   -> pmAltConSetElems (vi_neg vi)

isDataConSolution :: (PmAltCon, [TyVar], [Id]) -> Bool
isDataConSolution (PmAltConLike (RealDataCon _), _, _) = True
isDataConSolution _                                    = False

-- @lookupSolution nabla x@ picks a single solution ('vi_pos') of @x@ from
-- possibly many, preferring 'RealDataCon' solutions whenever possible.
lookupSolution :: Nabla -> Id -> Maybe (PmAltCon, [TyVar], [Id])
lookupSolution nabla x = case vi_pos (lookupVarInfo (nabla_tm_st nabla) x) of
  []                                         -> Nothing
  pos
    | Just sol <- find isDataConSolution pos -> Just sol
    | otherwise                              -> Just (head pos)

-------------------------------
-- * Adding facts to the oracle

-- | A term constraint.
data TmCt
  = TmVarCt     !Id !Id
  -- ^ @TmVarCt x y@ encodes "x ~ y", equating @x@ and @y@.
  | TmCoreCt    !Id !CoreExpr
  -- ^ @TmCoreCt x e@ encodes "x ~ e", equating @x@ with the 'CoreExpr' @e@.
  | TmConCt     !Id !PmAltCon ![TyVar] ![Id]
  -- ^ @TmConCt x K tvs ys@ encodes "x ~ K @tvs ys", equating @x@ with the 'PmAltCon'
  -- application @K @tvs ys@.
  | TmNotConCt  !Id !PmAltCon
  -- ^ @TmNotConCt x K@ encodes "x /~ K", asserting that @x@ can't be headed
  -- by @K@.
  | TmBotCt     !Id
  -- ^ @TmBotCt x@ encodes "x ~ ⊥", equating @x@ to ⊥.
  -- by @K@.
  | TmNotBotCt !Id
  -- ^ @TmNotBotCt x y@ encodes "x /~ ⊥", asserting that @x@ can't be ⊥.

instance Outputable TmCt where
  ppr (TmVarCt x y)            = ppr x <+> char '~' <+> ppr y
  ppr (TmCoreCt x e)           = ppr x <+> char '~' <+> ppr e
  ppr (TmConCt x con tvs args) = ppr x <+> char '~' <+> hsep (ppr con : pp_tvs ++ pp_args)
    where
      pp_tvs  = map ((<> char '@') . ppr) tvs
      pp_args = map ppr args
  ppr (TmNotConCt x con)       = ppr x <+> text "/~" <+> ppr con
  ppr (TmBotCt x)              = ppr x <+> text "~ ⊥"
  ppr (TmNotBotCt x)           = ppr x <+> text "/~ ⊥"

type TyCt = PredType

-- | An oracle constraint.
data PmCt
  = PmTyCt !TyCt
  -- ^ @PmTy pred_ty@ carries 'PredType's, for example equality constraints.
  | PmTmCt !TmCt
  -- ^ A term constraint.

type PmCts = Bag PmCt

pattern PmVarCt :: Id -> Id -> PmCt
pattern PmVarCt x y            = PmTmCt (TmVarCt x y)
pattern PmCoreCt :: Id -> CoreExpr -> PmCt
pattern PmCoreCt x e           = PmTmCt (TmCoreCt x e)
pattern PmConCt :: Id -> PmAltCon -> [TyVar] -> [Id] -> PmCt
pattern PmConCt x con tvs args = PmTmCt (TmConCt x con tvs args)
pattern PmNotConCt :: Id -> PmAltCon -> PmCt
pattern PmNotConCt x con       = PmTmCt (TmNotConCt x con)
pattern PmBotCt :: Id -> PmCt
pattern PmBotCt x              = PmTmCt (TmBotCt x)
pattern PmNotBotCt :: Id -> PmCt
pattern PmNotBotCt x           = PmTmCt (TmNotBotCt x)
{-# COMPLETE PmTyCt, PmVarCt, PmCoreCt, PmConCt, PmNotConCt, PmBotCt, PmNotBotCt #-}

instance Outputable PmCt where
  ppr (PmTyCt pred_ty) = ppr pred_ty
  ppr (PmTmCt tm_ct)   = ppr tm_ct

-- | Adds new constraints to 'Nabla' and returns 'Nothing' if that leads to a
-- contradiction.
addPmCts :: Nabla -> PmCts -> DsM (Maybe Nabla)
-- See Note [TmState invariants].
addPmCts nabla cts = do
  let (ty_cts, tm_cts) = partitionTyTmCts cts
  runSatisfiabilityCheck nabla $ mconcat
    [ tyIsSatisfiable True (listToBag ty_cts)
    , tmIsSatisfiable (listToBag tm_cts)
    ]

partitionTyTmCts :: PmCts -> ([TyCt], [TmCt])
partitionTyTmCts = partitionEithers . map to_either . toList
  where
    to_either (PmTyCt pred_ty) = Left pred_ty
    to_either (PmTmCt tm_ct)   = Right tm_ct

-- | Adds a single term constraint by dispatching to the various term oracle
-- functions.
addTmCt :: Nabla -> TmCt -> MaybeT DsM Nabla
addTmCt nabla (TmVarCt x y)            = addVarCt nabla x y
addTmCt nabla (TmCoreCt x e)           = addCoreCt nabla x e
addTmCt nabla (TmConCt x con tvs args) = addConCt nabla x con tvs args
addTmCt nabla (TmNotConCt x con)       = addNotConCt nabla x con
addTmCt nabla (TmBotCt x)              = addBotCt nabla x
addTmCt nabla (TmNotBotCt x)           = addNotBotCt nabla x

-- | Adds the constraint @x ~ ⊥@, e.g. that evaluation of a particular 'Id' @x@
-- surely diverges. Quite similar to 'addConCt', only that it only cares about
-- ⊥.
addBotCt :: Nabla -> Id -> MaybeT DsM Nabla
addBotCt nabla@MkNabla{ nabla_tm_st = TmSt env reps } x = do
  let (y, vi@VI { vi_bot = bot }) = lookupVarInfoNT (nabla_tm_st nabla) x
  case bot of
    IsNotBot -> mzero      -- There was x /~ ⊥. Contradiction!
    IsBot    -> pure nabla -- There already is x ~ ⊥. Nothing left to do
    MaybeBot -> do         -- We add x ~ ⊥
      let vi' = vi{ vi_bot = IsBot }
      pure nabla{ nabla_tm_st = TmSt (setEntrySDIE env y vi') reps}

-- | Record a @x ~/ K@ constraint, e.g. that a particular 'Id' @x@ can't
-- take the shape of a 'PmAltCon' @K@ in the 'Nabla' and return @Nothing@ if
-- that leads to a contradiction.
-- See Note [TmState invariants].
addNotConCt :: Nabla -> Id -> PmAltCon -> MaybeT DsM Nabla
addNotConCt _ _ (PmAltConLike (RealDataCon dc))
  | isNewDataCon dc = mzero -- (3) in Note [Coverage checking Newtype matches]
addNotConCt nabla@MkNabla{ nabla_tm_st = ts@(TmSt env reps) } x nalt = do
  let vi@(VI _ pos neg _ rcm) = lookupVarInfo ts x
  -- 1. Bail out quickly when nalt contradicts a solution
  let contradicts nalt (cl, _tvs, _args) = eqPmAltCon cl nalt == Equal
  guard (not (any (contradicts nalt) pos))
  -- 2. Only record the new fact when it's not already implied by one of the
  -- solutions
  let implies nalt (cl, _tvs, _args) = eqPmAltCon cl nalt == Disjoint
  let neg'
        | any (implies nalt) pos = neg
        -- See Note [Completeness checking with required Thetas]
        | hasRequiredTheta nalt  = neg
        | otherwise              = extendPmAltConSet neg nalt
  MASSERT( isPmAltConMatchStrict nalt )
  let vi1 = vi{ vi_neg = neg', vi_bot = IsNotBot }
  -- 3. Make sure there's at least one other possible constructor
  vi2 <- case nalt of
    PmAltConLike cl -> do
      rcm' <- lift (markMatched cl rcm)
      ensureInhabited nabla vi1{ vi_rcm = rcm' }
    _ ->
      pure vi1
  pure nabla{ nabla_tm_st = TmSt (setEntrySDIE env x vi2) reps }

hasRequiredTheta :: PmAltCon -> Bool
hasRequiredTheta (PmAltConLike cl) = notNull req_theta
  where
    (_,_,_,_,req_theta,_,_) = conLikeFullSig cl
hasRequiredTheta _                 = False

{- Note [Completeness checking with required Thetas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the situation in #11224

    import Text.Read (readMaybe)
    pattern PRead :: Read a => () => a -> String
    pattern PRead x <- (readMaybe -> Just x)
    f :: String -> Int
    f (PRead x)  = x
    f (PRead xs) = length xs
    f _          = 0

Is the first match exhaustive on the PRead synonym? Should the second line thus
deemed redundant? The answer is, of course, No! The required theta is like a
hidden parameter which must be supplied at the pattern match site, so PRead
is much more like a view pattern (where behavior depends on the particular value
passed in).
The simple solution here is to forget in 'addNotConCt' that we matched
on synonyms with a required Theta like @PRead@, so that subsequent matches on
the same constructor are never flagged as redundant. The consequence is that
we no longer detect the actually redundant match in

    g :: String -> Int
    g (PRead x) = x
    g (PRead y) = y -- redundant!
    g _         = 0

But that's a small price to pay, compared to the proper solution here involving
storing required arguments along with the PmAltConLike in 'vi_neg'.
-}

-- | Guess the universal argument types of a ConLike from an instantiation of
-- its result type. Rather easy for DataCons, but not so much for PatSynCons.
-- See Note [Pattern synonym result type] in "GHC.Core.PatSyn".
guessConLikeUnivTyArgsFromResTy :: FamInstEnvs -> Type -> ConLike -> Maybe [Type]
guessConLikeUnivTyArgsFromResTy env res_ty (RealDataCon dc) = do
  (tc, tc_args) <- splitTyConApp_maybe res_ty
  -- Consider data families: In case of a DataCon, we need to translate to
  -- the representation TyCon. For PatSyns, they are relative to the data
  -- family TyCon, so we don't need to translate them.
  let (rep_tc, tc_args', _) = tcLookupDataFamInst env tc tc_args
  if rep_tc == dataConTyCon dc
    then Just tc_args'
    else Nothing
guessConLikeUnivTyArgsFromResTy _   res_ty (PatSynCon ps)  = do
  -- We are successful if we managed to instantiate *every* univ_tv of con.
  -- This is difficult and bound to fail in some cases, see
  -- Note [Pattern synonym result type] in GHC.Core.PatSyn. So we just try our best
  -- here and be sure to return an instantiation when we can substitute every
  -- universally quantified type variable.
  -- We *could* instantiate all the other univ_tvs just to fresh variables, I
  -- suppose, but that means we get weird field types for which we don't know
  -- anything. So we prefer to keep it simple here.
  let (univ_tvs,_,_,_,_,con_res_ty) = patSynSig ps
  subst <- tcMatchTy con_res_ty res_ty
  traverse (lookupTyVar subst) univ_tvs

-- | Adds the constraint @x ~/ ⊥@ to 'Nabla'. Quite similar to 'addNotConCt',
-- but only cares for the ⊥ "constructor".
addNotBotCt :: Nabla -> Id -> MaybeT DsM Nabla
addNotBotCt nabla@MkNabla{ nabla_tm_st = TmSt env reps } x = do
  let (y, vi@VI { vi_bot = bot }) = lookupVarInfoNT (nabla_tm_st nabla) x
  case bot of
    IsBot    -> mzero      -- There was x ~ ⊥. Contradiction!
    IsNotBot -> pure nabla -- There already is x /~ ⊥. Nothing left to do
    MaybeBot -> do         -- We add x /~ ⊥ and test if x is still inhabited
      vi <- ensureInhabited nabla vi{ vi_bot = IsNotBot }
      pure nabla{ nabla_tm_st = TmSt (setEntrySDIE env y vi) reps}

-- | Returns (Just vi) if at least one member of each ConLike in the COMPLETE
-- set satisfies the oracle
--
-- Internally uses and updates the ConLikeSets in vi_rcm.
--
-- NB: Does /not/ filter each ConLikeSet with the oracle; members may
--     remain that do not statisfy it.  This lazy approach just
--     avoids doing unnecessary work.
ensureInhabited :: Nabla -> VarInfo -> MaybeT DsM VarInfo
ensureInhabited nabla vi = case vi_bot vi of
  MaybeBot -> pure vi -- The |-Bot rule from the paper
  IsBot    -> pure vi
  IsNotBot -> lift (add_matches vi) >>= inst_complete_sets
  where
    add_matches :: VarInfo -> DsM VarInfo
    add_matches vi = do
      res <- pmTopNormaliseType (nabla_ty_st nabla) (vi_ty vi)
      rcm <- case reprTyCon_maybe (normalisedSourceType res) of
        Just tc -> addTyConMatches tc (vi_rcm vi)
        Nothing -> addCompleteMatches (vi_rcm vi)
      pure vi{ vi_rcm = rcm }

    reprTyCon_maybe :: Type -> Maybe TyCon
    reprTyCon_maybe ty = case splitTyConApp_maybe ty of
      Nothing          -> Nothing
      Just (tc, _args) -> case tyConFamInst_maybe tc of
        Nothing          -> Just tc
        Just (tc_fam, _) -> Just tc_fam

    -- | This is the |-Inst rule from the paper (section 4.5). Tries to
    -- find an inhabitant in every complete set by instantiating with one their
    -- constructors. If there is any complete set where we can't find an
    -- inhabitant, the whole thing is uninhabited.
    -- See also Note [Implementation of COMPLETE pragmas].
    inst_complete_sets :: VarInfo -> MaybeT DsM VarInfo
    inst_complete_sets vi@VI{ vi_rcm = rcm } = do
      rcm' <- trvRcm (\cls -> inst_complete_set vi cls (uniqDSetToList cls)) rcm
      pure vi{ vi_rcm = rcm' }

    inst_complete_set :: VarInfo -> ConLikeSet -> [ConLike] -> MaybeT DsM ConLikeSet
    -- (inst_complete_set cs cls) iterates over cls, deleting from cs
    -- any uninhabited elements of cls.  Stop (returning Just cs)
    -- when you see an inhabited element; return Nothing if all
    -- are uninhabited
    inst_complete_set _ _  [] = mzero
    inst_complete_set vi cs (con:cons) = lift (inst_and_test vi con) >>= \case
      True  -> pure cs
      False -> inst_complete_set vi (delOneFromUniqDSet cs con) cons

    inst_and_test :: VarInfo -> ConLike -> DsM Bool
    -- @inst_and_test K@ Returns False if a non-bottom value @v::ty@ cannot possibly
    -- be of form @K _ _ _@. Returning True is always sound.
    --
    -- It's like 'DataCon.dataConCannotMatch', but more clever because it takes
    -- the facts in Nabla into account.
    inst_and_test vi con = do
      env <- dsGetFamInstEnvs
      case guessConLikeUnivTyArgsFromResTy env (vi_ty vi) con of
        Nothing -> pure True -- be conservative about this
        Just arg_tys -> do
          (_tvs, _vars, ty_cs, strict_arg_tys) <- mkOneConFull arg_tys con
          tracePm "inst_and_test" (ppr con $$ ppr ty_cs)
          -- No need to run the term oracle compared to pmIsSatisfiable
          fmap isJust <$> runSatisfiabilityCheck nabla $ mconcat
            -- Important to pass False to tyIsSatisfiable here, so that we won't
            -- recursively call ensureAllInhabited, leading to an
            -- endless recursion.
            [ tyIsSatisfiable False ty_cs
            , tysAreNonVoid initRecTc strict_arg_tys
            ]

-- | Checks if every 'VarInfo' in the term oracle has still an inhabited
-- 'vi_rcm', considering the current type information in 'Nabla'.
-- This check is necessary after having matched on a GADT con to weed out
-- impossible matches.
ensureAllInhabited :: Nabla -> DsM (Maybe Nabla)
ensureAllInhabited nabla@MkNabla{ nabla_tm_st = TmSt env reps }
  = runMaybeT (set_tm_cs_env nabla <$> traverseSDIE go env)
  where
    set_tm_cs_env nabla env = nabla{ nabla_tm_st = TmSt env reps }
    go vi = ensureInhabited nabla vi

--------------------------------------
-- * Term oracle unification procedure

-- | Adds a @x ~ y@ constraint by trying to unify two 'Id's and record the
-- gained knowledge in 'Nabla'.
--
-- Returns @Nothing@ when there's a contradiction. Returns @Just nabla@
-- when the constraint was compatible with prior facts, in which case @nabla@
-- has integrated the knowledge from the equality constraint.
--
-- See Note [TmState invariants].
addVarCt :: Nabla -> Id -> Id -> MaybeT DsM Nabla
addVarCt nabla@MkNabla{ nabla_tm_st = TmSt env _ } x y
  -- It's important that we never @equate@ two variables of the same equivalence
  -- class, otherwise we might get cyclic substitutions.
  -- Cf. 'extendSubstAndSolve' and
  -- @testsuite/tests/pmcheck/should_compile/CyclicSubst.hs@.
  | sameRepresentativeSDIE env x y = pure nabla
  | otherwise                      = equate nabla x y

-- | @equate ts@(TmSt env) x y@ merges the equivalence classes of @x@ and @y@ by
-- adding an indirection to the environment.
-- Makes sure that the positive and negative facts of @x@ and @y@ are
-- compatible.
-- Preconditions: @not (sameRepresentativeSDIE env x y)@
--
-- See Note [TmState invariants].
equate :: Nabla -> Id -> Id -> MaybeT DsM Nabla
equate nabla@MkNabla{ nabla_tm_st = TmSt env reps } x y
  = ASSERT( not (sameRepresentativeSDIE env x y) )
    case (lookupSDIE env x, lookupSDIE env y) of
      (Nothing, _) -> pure (nabla{ nabla_tm_st = TmSt (setIndirectSDIE env x y) reps })
      (_, Nothing) -> pure (nabla{ nabla_tm_st = TmSt (setIndirectSDIE env y x) reps })
      -- Merge the info we have for x into the info for y
      (Just vi_x, Just vi_y) -> do
        -- This assert will probably trigger at some point...
        -- We should decide how to break the tie
        MASSERT2( vi_ty vi_x `eqType` vi_ty vi_y, text "Not same type" )
        -- First assume that x and y are in the same equivalence class
        let env_ind = setIndirectSDIE env x y
        -- Then sum up the refinement counters
        let env_refs = setEntrySDIE env_ind y vi_y
        let nabla_refs = nabla{ nabla_tm_st = TmSt env_refs reps }
        -- and then gradually merge every positive fact we have on x into y
        let add_fact nabla (cl, tvs, args) = addConCt nabla y cl tvs args
        nabla_pos <- foldlM add_fact nabla_refs (vi_pos vi_x)
        -- Do the same for negative info
        let add_refut nabla nalt = addNotConCt nabla y nalt
        nabla_neg <- foldlM add_refut nabla_pos (pmAltConSetElems (vi_neg vi_x))
        -- vi_rcm will be updated in addNotConCt, so we are good to
        -- go!
        pure nabla_neg

-- | Add a @x ~ K tvs args ts@ constraint.
-- @addConCt x K tvs args ts@ extends the substitution with a solution
-- @x :-> (K, tvs, args)@ if compatible with the negative and positive info we
-- have on @x@, reject (@Nothing@) otherwise.
--
-- See Note [TmState invariants].
addConCt :: Nabla -> Id -> PmAltCon -> [TyVar] -> [Id] -> MaybeT DsM Nabla
addConCt nabla@MkNabla{ nabla_tm_st = ts@(TmSt env reps) } x alt tvs args = do
  let VI ty pos neg bot rcm = lookupVarInfo ts x
  -- First try to refute with a negative fact
  guard (not (elemPmAltConSet alt neg))
  -- Then see if any of the other solutions (remember: each of them is an
  -- additional refinement of the possible values x could take) indicate a
  -- contradiction
  guard (all ((/= Disjoint) . eqPmAltCon alt . fstOf3) pos)
  -- Now we should be good! Add (alt, tvs, args) as a possible solution, or
  -- refine an existing one
  case find ((== Equal) . eqPmAltCon alt . fstOf3) pos of
    Just (_con, other_tvs, other_args) -> do
      -- We must unify existentially bound ty vars and arguments!
      let ty_cts = equateTys (map mkTyVarTy tvs) (map mkTyVarTy other_tvs)
      when (length args /= length other_args) $
        lift $ tracePm "error" (ppr x <+> ppr alt <+> ppr args <+> ppr other_args)
      let tm_cts = zipWithEqual "addConCt" PmVarCt args other_args
      MaybeT $ addPmCts nabla (listToBag ty_cts `unionBags` listToBag tm_cts)
    Nothing -> do
      let pos' = (alt, tvs, args):pos
      let nabla_with bot =
            nabla{ nabla_tm_st = TmSt (setEntrySDIE env x (VI ty pos' neg bot rcm)) reps}
      -- Do (2) in Note [Coverage checking Newtype matches]
      case (alt, args) of
        (PmAltConLike (RealDataCon dc), [y]) | isNewDataCon dc ->
          case bot of
            MaybeBot -> pure (nabla_with MaybeBot)
            IsBot    -> addBotCt (nabla_with MaybeBot) y
            IsNotBot -> addNotBotCt (nabla_with MaybeBot) y
        _ -> ASSERT( isPmAltConMatchStrict alt )
             pure (nabla_with IsNotBot) -- strict match ==> not ⊥

equateTys :: [Type] -> [Type] -> [PmCt]
equateTys ts us =
  [ PmTyCt (mkPrimEqPred t u)
  | (t, u) <- zipEqual "equateTys" ts us
  -- The following line filters out trivial Refl constraints, so that we don't
  -- need to initialise the type oracle that often
  , not (eqType t u)
  ]

----------------------------------------
-- * Enumerating inhabitation candidates

-- | Information about a conlike that is relevant to coverage checking.
-- It is called an \"inhabitation candidate\" since it is a value which may
-- possibly inhabit some type, but only if its term constraints ('ic_tm_cs')
-- and type constraints ('ic_ty_cs') are permitting, and if all of its strict
-- argument types ('ic_strict_arg_tys') are inhabitable.
-- See @Note [Strict argument type constraints]@.
data InhabitationCandidate =
  InhabitationCandidate
  { ic_cs             :: PmCts
  , ic_strict_arg_tys :: [Type]
  }

instance Outputable InhabitationCandidate where
  ppr (InhabitationCandidate cs strict_arg_tys) =
    text "InhabitationCandidate" <+>
      vcat [ text "ic_cs             =" <+> ppr cs
           , text "ic_strict_arg_tys =" <+> ppr strict_arg_tys ]

mkInhabitationCandidate :: Id -> DataCon -> DsM InhabitationCandidate
-- Precondition: idType x is a TyConApp, so that tyConAppArgs in here is safe.
mkInhabitationCandidate x dc = do
  let cl = RealDataCon dc
  let tc_args = tyConAppArgs (idType x)
  (ty_vars, arg_vars, ty_cs, strict_arg_tys) <- mkOneConFull tc_args cl
  pure InhabitationCandidate
        { ic_cs = PmTyCt <$> ty_cs `snocBag` PmConCt x (PmAltConLike cl) ty_vars arg_vars
        , ic_strict_arg_tys = strict_arg_tys
        }

-- | Generate all 'InhabitationCandidate's for a given type. The result is
-- either @'Left' ty@, if the type cannot be reduced to a closed algebraic type
-- (or if it's one trivially inhabited, like 'Int'), or @'Right' candidates@,
-- if it can. In this case, the candidates are the signature of the tycon, each
-- one accompanied by the term- and type- constraints it gives rise to.
-- See also Note [Checking EmptyCase Expressions]
inhabitationCandidates :: Nabla -> Type
                       -> DsM (Either Type (TyCon, Id, [InhabitationCandidate]))
inhabitationCandidates MkNabla{ nabla_ty_st = ty_st } ty = do
  pmTopNormaliseType ty_st ty >>= \case
    NoChange _                    -> alts_to_check ty     ty      []
    NormalisedByConstraints ty'   -> alts_to_check ty'    ty'     []
    HadRedexes src_ty dcs core_ty -> alts_to_check src_ty core_ty dcs
  where
    build_newtype :: (Type, DataCon, Type) -> Id -> DsM (Id, PmCt)
    build_newtype (ty, dc, _arg_ty) x = do
      -- ty is the type of @dc x@. It's a @dataConTyCon dc@ application.
      y <- mkPmId ty
      -- Newtypes don't have existentials (yet?!), so passing an empty list as
      -- ex_tvs.
      pure (y, PmConCt y (PmAltConLike (RealDataCon dc)) [] [x])

    build_newtypes :: Id -> [(Type, DataCon, Type)] -> DsM (Id, [PmCt])
    build_newtypes x = foldrM (\dc (x, cts) -> go dc x cts) (x, [])
      where
        go dc x cts = second (:cts) <$> build_newtype dc x

    -- Inhabitation candidates, using the result of pmTopNormaliseType
    alts_to_check :: Type -> Type -> [(Type, DataCon, Type)]
                  -> DsM (Either Type (TyCon, Id, [InhabitationCandidate]))
    alts_to_check src_ty core_ty dcs = case splitTyConApp_maybe core_ty of
      Just (tc, _)
        |  isTyConTriviallyInhabited tc
        -> case dcs of
             []    -> return (Left src_ty)
             (_:_) -> do inner <- mkPmId core_ty
                         (outer, new_tm_cts) <- build_newtypes inner dcs
                         return $ Right (tc, outer, [InhabitationCandidate
                           { ic_cs = listToBag new_tm_cts
                           , ic_strict_arg_tys = [] }])

        |  pmIsClosedType core_ty && not (isAbstractTyCon tc)
           -- Don't consider abstract tycons since we don't know what their
           -- constructors are, which makes the results of coverage checking
           -- them extremely misleading.
        -> do
             inner <- mkPmId core_ty -- it would be wrong to unify inner
             alts <- mapM (mkInhabitationCandidate inner) (tyConDataCons tc)
             (outer, new_cts) <- build_newtypes inner dcs
             let wrap_dcs alt = alt{ ic_cs = listToBag new_cts `unionBags` ic_cs alt}
             return $ Right (tc, outer, map wrap_dcs alts)
      -- For other types conservatively assume that they are inhabited.
      _other -> return (Left src_ty)

-- | All these types are trivially inhabited
triviallyInhabitedTyCons :: UniqSet TyCon
triviallyInhabitedTyCons = mkUniqSet [
    charTyCon, doubleTyCon, floatTyCon, intTyCon, wordTyCon, word8TyCon
  ]

isTyConTriviallyInhabited :: TyCon -> Bool
isTyConTriviallyInhabited tc = elementOfUniqSet tc triviallyInhabitedTyCons

----------------------------
-- * Detecting vacuous types

{- Note [Checking EmptyCase Expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Empty case expressions are strict on the scrutinee. That is, `case x of {}`
will force argument `x`. Hence, `covCheckMatches` is not sufficient for checking
empty cases, because it assumes that the match is not strict (which is true
for all other cases, apart from EmptyCase). This gave rise to #10746. Instead,
we do the following:

1. We normalise the outermost type family redex, data family redex or newtype,
   using pmTopNormaliseType (in "GHC.Core.FamInstEnv"). This computes 3
   things:
   (a) A normalised type src_ty, which is equal to the type of the scrutinee in
       source Haskell (does not normalise newtypes or data families)
   (b) The actual normalised type core_ty, which coincides with the result
       topNormaliseType_maybe. This type is not necessarily equal to the input
       type in source Haskell. And this is precicely the reason we compute (a)
       and (c): the reasoning happens with the underlying types, but both the
       patterns and types we print should respect newtypes and also show the
       family type constructors and not the representation constructors.

   (c) A list of all newtype data constructors dcs, each one corresponding to a
       newtype rewrite performed in (b).

   For an example see also Note [Type normalisation]
   in "GHC.Core.FamInstEnv".

2. Function Check.checkEmptyCase' performs the check:
   - If core_ty is not an algebraic type, then we cannot check for
     inhabitation, so we emit (_ :: src_ty) as missing, conservatively assuming
     that the type is inhabited.
   - If core_ty is an algebraic type, then we unfold the scrutinee to all
     possible constructor patterns, using inhabitationCandidates, and then
     check each one for constraint satisfiability, same as we do for normal
     pattern match checking.
-}

-- | A 'SatisfiabilityCheck' based on "NonVoid ty" constraints, e.g. Will
-- check if the @strict_arg_tys@ are actually all inhabited.
-- Returns the old 'Nabla' if all the types are non-void according to 'Nabla'.
tysAreNonVoid :: RecTcChecker -> [Type] -> SatisfiabilityCheck
tysAreNonVoid rec_env strict_arg_tys = SC $ \nabla -> do
  all_non_void <- checkAllNonVoid rec_env nabla strict_arg_tys
  -- Check if each strict argument type is inhabitable
  pure $ if all_non_void
            then Just nabla
            else Nothing

-- | Implements two performance optimizations, as described in
-- @Note [Strict argument type constraints]@.
checkAllNonVoid :: RecTcChecker -> Nabla -> [Type] -> DsM Bool
checkAllNonVoid rec_ts amb_cs strict_arg_tys = do
  let definitely_inhabited = definitelyInhabitedType (nabla_ty_st amb_cs)
  tys_to_check <- filterOutM definitely_inhabited strict_arg_tys
  -- See Note [Fuel for the inhabitation test]
  let rec_max_bound | tys_to_check `lengthExceeds` 1
                    = 1
                    | otherwise
                    = 3
      rec_ts' = setRecTcMaxBound rec_max_bound rec_ts
  allM (nonVoid rec_ts' amb_cs) tys_to_check

-- | Checks if a strict argument type of a conlike is inhabitable by a
-- terminating value (i.e, an 'InhabitationCandidate').
-- See @Note [Strict argument type constraints]@.
nonVoid
  :: RecTcChecker -- ^ The per-'TyCon' recursion depth limit.
  -> Nabla        -- ^ The ambient term/type constraints (known to be
                  --   satisfiable).
  -> Type         -- ^ The strict argument type.
  -> DsM Bool     -- ^ 'True' if the strict argument type might be inhabited by
                  --   a terminating value (i.e., an 'InhabitationCandidate').
                  --   'False' if it is definitely uninhabitable by anything
                  --   (except bottom).
nonVoid rec_ts amb_cs strict_arg_ty = do
  mb_cands <- inhabitationCandidates amb_cs strict_arg_ty
  case mb_cands of
    Right (tc, _, cands)
      -- See Note [Fuel for the inhabitation test]
      |  Just rec_ts' <- checkRecTc rec_ts tc
      -> anyM (cand_is_inhabitable rec_ts' amb_cs) cands
           -- A strict argument type is inhabitable by a terminating value if
           -- at least one InhabitationCandidate is inhabitable.
    _ -> pure True
           -- Either the type is trivially inhabited or we have exceeded the
           -- recursion depth for some TyCon (so bail out and conservatively
           -- claim the type is inhabited).
  where
    -- Checks if an InhabitationCandidate for a strict argument type:
    --
    -- (1) Has satisfiable term and type constraints.
    -- (2) Has 'nonVoid' strict argument types (we bail out of this
    --     check if recursion is detected).
    --
    -- See Note [Strict argument type constraints]
    cand_is_inhabitable :: RecTcChecker -> Nabla
                        -> InhabitationCandidate -> DsM Bool
    cand_is_inhabitable rec_ts amb_cs
      (InhabitationCandidate{ ic_cs             = new_cs
                            , ic_strict_arg_tys = new_strict_arg_tys }) = do
        let (new_ty_cs, new_tm_cs) = partitionTyTmCts new_cs
        fmap isJust $ runSatisfiabilityCheck amb_cs $ mconcat
          [ tyIsSatisfiable False (listToBag new_ty_cs)
          , tmIsSatisfiable (listToBag new_tm_cs)
          , tysAreNonVoid rec_ts new_strict_arg_tys
          ]

-- | @'definitelyInhabitedType' ty@ returns 'True' if @ty@ has at least one
-- constructor @C@ such that:
--
-- 1. @C@ has no equality constraints.
-- 2. @C@ has no strict argument types.
--
-- See the @Note [Strict argument type constraints]@.
definitelyInhabitedType :: TyState -> Type -> DsM Bool
definitelyInhabitedType ty_st ty = do
  res <- pmTopNormaliseType ty_st ty
  pure $ case res of
           HadRedexes _ cons _ -> any meets_criteria cons
           _                   -> False
  where
    meets_criteria :: (Type, DataCon, Type) -> Bool
    meets_criteria (_, con, _) =
      null (dataConEqSpec con) && -- (1)
      null (dataConImplBangs con) -- (2)

{- Note [Strict argument type constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the ConVar case of clause processing, each conlike K traditionally
generates two different forms of constraints:

* A term constraint (e.g., x ~ K y1 ... yn)
* Type constraints from the conlike's context (e.g., if K has type
  forall bs. Q => s1 .. sn -> T tys, then Q would be its type constraints)

As it turns out, these alone are not enough to detect a certain class of
unreachable code. Consider the following example (adapted from #15305):

  data K = K1 | K2 !Void

  f :: K -> ()
  f K1 = ()

Even though `f` doesn't match on `K2`, `f` is exhaustive in its patterns. Why?
Because it's impossible to construct a terminating value of type `K` using the
`K2` constructor, and thus it's impossible for `f` to ever successfully match
on `K2`.

The reason is because `K2`'s field of type `Void` is //strict//. Because there
are no terminating values of type `Void`, any attempt to construct something
using `K2` will immediately loop infinitely or throw an exception due to the
strictness annotation. (If the field were not strict, then `f` could match on,
say, `K2 undefined` or `K2 (let x = x in x)`.)

Since neither the term nor type constraints mentioned above take strict
argument types into account, we make use of the `nonVoid` function to
determine whether a strict type is inhabitable by a terminating value or not.
We call this the "inhabitation test".

`nonVoid ty` returns True when either:
1. `ty` has at least one InhabitationCandidate for which both its term and type
   constraints are satisfiable, and `nonVoid` returns `True` for all of the
   strict argument types in that InhabitationCandidate.
2. We're unsure if it's inhabited by a terminating value.

`nonVoid ty` returns False when `ty` is definitely uninhabited by anything
(except bottom). Some examples:

* `nonVoid Void` returns False, since Void has no InhabitationCandidates.
  (This is what lets us discard the `K2` constructor in the earlier example.)
* `nonVoid (Int :~: Int)` returns True, since it has an InhabitationCandidate
  (through the Refl constructor), and its term constraint (x ~ Refl) and
  type constraint (Int ~ Int) are satisfiable.
* `nonVoid (Int :~: Bool)` returns False. Although it has an
  InhabitationCandidate (by way of Refl), its type constraint (Int ~ Bool) is
  not satisfiable.
* Given the following definition of `MyVoid`:

    data MyVoid = MkMyVoid !Void

  `nonVoid MyVoid` returns False. The InhabitationCandidate for the MkMyVoid
  constructor contains Void as a strict argument type, and since `nonVoid Void`
  returns False, that InhabitationCandidate is discarded, leaving no others.
* Whether or not a type is inhabited is undecidable in general.
  See Note [Fuel for the inhabitation test].
* For some types, inhabitation is evident immediately and we don't need to
  perform expensive tests. See Note [Types that are definitely inhabitable].

Note [Fuel for the inhabitation test]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Whether or not a type is inhabited is undecidable in general. As a result, we
can run into infinite loops in `nonVoid`. Therefore, we adopt a fuel-based
approach to prevent that.

Consider the following example:

  data Abyss = MkAbyss !Abyss
  stareIntoTheAbyss :: Abyss -> a
  stareIntoTheAbyss x = case x of {}

In principle, stareIntoTheAbyss is exhaustive, since there is no way to
construct a terminating value using MkAbyss. However, both the term and type
constraints for MkAbyss are satisfiable, so the only way one could determine
that MkAbyss is unreachable is to check if `nonVoid Abyss` returns False.
There is only one InhabitationCandidate for Abyss—MkAbyss—and both its term
and type constraints are satisfiable, so we'd need to check if `nonVoid Abyss`
returns False... and now we've entered an infinite loop!

To avoid this sort of conundrum, `nonVoid` uses a simple test to detect the
presence of recursive types (through `checkRecTc`), and if recursion is
detected, we bail out and conservatively assume that the type is inhabited by
some terminating value. This avoids infinite loops at the expense of making
the coverage checker incomplete with respect to functions like
stareIntoTheAbyss above. Then again, the same problem occurs with recursive
newtypes, like in the following code:

  newtype Chasm = MkChasm Chasm
  gazeIntoTheChasm :: Chasm -> a
  gazeIntoTheChasm x = case x of {} -- Erroneously warned as non-exhaustive

So this limitation is somewhat understandable.

Note that even with this recursion detection, there is still a possibility that
`nonVoid` can run in exponential time. Consider the following data type:

  data T = MkT !T !T !T

If we call `nonVoid` on each of its fields, that will require us to once again
check if `MkT` is inhabitable in each of those three fields, which in turn will
require us to check if `MkT` is inhabitable again... As you can see, the
branching factor adds up quickly, and if the recursion depth limit is, say,
100, then `nonVoid T` will effectively take forever.

To mitigate this, we check the branching factor every time we are about to call
`nonVoid` on a list of strict argument types. If the branching factor exceeds 1
(i.e., if there is potential for exponential runtime), then we limit the
maximum recursion depth to 1 to mitigate the problem. If the branching factor
is exactly 1 (i.e., we have a linear chain instead of a tree), then it's okay
to stick with a larger maximum recursion depth.

In #17977 we saw that the defaultRecTcMaxBound (100 at the time of writing) was
too large and had detrimental effect on performance of the coverage checker.
Given that we only commit to a best effort anyway, we decided to substantially
decrement the recursion depth to 3, at the cost of precision in some edge cases
like

  data Nat = Z | S Nat
  data Down :: Nat -> Type where
    Down :: !(Down n) -> Down (S n)
  f :: Down (S (S (S (S (S Z))))) -> ()
  f x = case x of {}

Since the coverage won't bother to instantiate Down 4 levels deep to see that it
is in fact uninhabited, it will emit a inexhaustivity warning for the case.

Note [Types that are definitely inhabitable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Another microoptimization applies to data types like this one:

  data S a = S ![a] !T

Even though there is a strict field of type [a], it's quite silly to call
nonVoid on it, since it's "obvious" that it is inhabitable. To make this
intuition formal, we say that a type is definitely inhabitable (DI) if:

  * It has at least one constructor C such that:
    1. C has no equality constraints (since they might be unsatisfiable)
    2. C has no strict argument types (since they might be uninhabitable)

It's relatively cheap to check if a type is DI, so before we call `nonVoid`
on a list of strict argument types, we filter out all of the DI ones.
-}

--------------------------------------------
-- * Providing positive evidence for a Nabla

-- | @provideEvidence vs n nabla@ returns a list of
-- at most @n@ (but perhaps empty) refinements of @nabla@ that instantiate
-- @vs@ to compatible constructor applications or wildcards.
-- Negative information is only retained if literals are involved or when
-- for recursive GADTs.
provideEvidence :: [Id] -> Int -> Nabla -> DsM [Nabla]
provideEvidence = go
  where
    go _      0 _     = pure []
    go []     _ nabla = pure [nabla]
    go (x:xs) n nabla = do
      tracePm "provideEvidence" (ppr x $$ ppr xs $$ ppr nabla $$ ppr n)
      let VI _ pos neg _ _ = lookupVarInfo (nabla_tm_st nabla) x
      case pos of
        _:_ -> do
          -- All solutions must be valid at once. Try to find candidates for their
          -- fields. Example:
          --   f x@(Just _) True = case x of SomePatSyn _ -> ()
          -- after this clause, we want to report that
          --   * @f Nothing _@ is uncovered
          --   * @f x False@ is uncovered
          -- where @x@ will have two possibly compatible solutions, @Just y@ for
          -- some @y@ and @SomePatSyn z@ for some @z@. We must find evidence for @y@
          -- and @z@ that is valid at the same time. These constitute arg_vas below.
          let arg_vas = concatMap (\(_cl, _tvs, args) -> args) pos
          go (arg_vas ++ xs) n nabla
        []
          -- When there are literals involved, just print negative info
          -- instead of listing missed constructors
          | notNull [ l | PmAltLit l <- pmAltConSetElems neg ]
          -> go xs n nabla
        [] -> try_instantiate x xs n nabla

    -- | Tries to instantiate a variable by possibly following the chain of
    -- newtypes and then instantiating to all ConLikes of the wrapped type's
    -- minimal residual COMPLETE set.
    try_instantiate :: Id -> [Id] -> Int -> Nabla -> DsM [Nabla]
    -- Convention: x binds the outer constructor in the chain, y the inner one.
    try_instantiate x xs n nabla = do
      (_src_ty, dcs, rep_ty) <- tntrGuts <$> pmTopNormaliseType (nabla_ty_st nabla) (idType x)
      let build_newtype (x, nabla) (_ty, dc, arg_ty) = do
            y <- lift $ mkPmId arg_ty
            -- Newtypes don't have existentials (yet?!), so passing an empty
            -- list as ex_tvs.
            nabla' <- addConCt nabla x (PmAltConLike (RealDataCon dc)) [] [y]
            pure (y, nabla')
      runMaybeT (foldlM build_newtype (x, nabla) dcs) >>= \case
        Nothing -> pure []
        Just (y, newty_nabla) -> do
          -- Pick a COMPLETE set and instantiate it (n at max). Take care of ⊥.
          let vi = lookupVarInfo (nabla_tm_st newty_nabla) y
          rcm <- case splitTyConApp_maybe rep_ty of
                Nothing      -> pure (vi_rcm vi)
                Just (tc, _) -> addTyConMatches tc (vi_rcm vi)
          mb_cls <- pickMinimalCompleteSet rep_ty rcm
          case uniqDSetToList <$> mb_cls of
            Just cls -> do
              nablas <- instantiate_cons y rep_ty xs n newty_nabla cls
              if null nablas && vi_bot vi /= IsNotBot
                then go xs n newty_nabla -- bot is still possible. Display a wildcard!
                else pure nablas
            Nothing -> go xs n newty_nabla -- no COMPLETE sets ==> inhabited

    instantiate_cons :: Id -> Type -> [Id] -> Int -> Nabla -> [ConLike] -> DsM [Nabla]
    instantiate_cons _ _  _  _ _     []       = pure []
    instantiate_cons _ _  _  0 _     _        = pure []
    instantiate_cons _ ty xs n nabla _
      -- We don't want to expose users to GHC-specific constructors for Int etc.
      | fmap (isTyConTriviallyInhabited . fst) (splitTyConApp_maybe ty) == Just True
      = go xs n nabla
    instantiate_cons x ty xs n nabla (cl:cls) = do
      env <- dsGetFamInstEnvs
      case guessConLikeUnivTyArgsFromResTy env ty cl of
        Nothing -> pure [nabla] -- No idea how to refine this one, so just finish off with a wildcard
        Just arg_tys -> do
          (tvs, arg_vars, new_ty_cs, strict_arg_tys) <- mkOneConFull arg_tys cl
          let new_tm_cs = unitBag (TmConCt x (PmAltConLike cl) tvs arg_vars)
          -- Now check satifiability
          mb_nabla <- pmIsSatisfiable nabla new_ty_cs new_tm_cs strict_arg_tys
          tracePm "instantiate_cons" (vcat [ ppr x
                                           , ppr (idType x)
                                           , ppr ty
                                           , ppr cl
                                           , ppr arg_tys
                                           , ppr new_tm_cs
                                           , ppr new_ty_cs
                                           , ppr strict_arg_tys
                                           , ppr nabla
                                           , ppr mb_nabla
                                           , ppr n ])
          con_nablas <- case mb_nabla of
            Nothing     -> pure []
            -- NB: We don't prepend arg_vars as we don't have any evidence on
            -- them and we only want to split once on a data type. They are
            -- inhabited, otherwise pmIsSatisfiable would have refuted.
            Just nabla' -> go xs n nabla'
          other_cons_nablas <- instantiate_cons x ty xs (n - length con_nablas) nabla cls
          pure (con_nablas ++ other_cons_nablas)

pickMinimalCompleteSet :: Type -> ResidualCompleteMatches -> DsM (Maybe ConLikeSet)
pickMinimalCompleteSet ty rcm = do
  env <- dsGetFamInstEnvs
  pure $ case filter (all (is_valid env) . uniqDSetToList) (getRcm rcm) of
    []    -> Nothing
    clss' -> Just (minimumBy (comparing sizeUniqDSet) clss')
  where
    is_valid :: FamInstEnvs -> ConLike -> Bool
    is_valid env cl = isJust (guessConLikeUnivTyArgsFromResTy env ty cl)

-- | Finds a representant of the semantic equality class of the given @e@.
-- Which is the @x@ of a @let x = e'@ constraint (with @e@ semantically
-- equivalent to @e'@) we encountered earlier, or a fresh identifier if
-- there weren't any such constraints.
representCoreExpr :: Nabla -> CoreExpr -> DsM (Nabla, Id)
representCoreExpr nabla@MkNabla{ nabla_tm_st = ts@TmSt{ ts_reps = reps } } e
  | Just rep <- lookupCoreMap reps e = pure (nabla, rep)
  | otherwise = do
      rep <- mkPmId (exprType e)
      let reps'  = extendCoreMap reps e rep
      let nabla' = nabla{ nabla_tm_st = ts{ ts_reps = reps' } }
      pure (nabla', rep)

-- | Inspects a 'PmCoreCt' @let x = e@ by recording constraints for @x@ based
-- on the shape of the 'CoreExpr' @e@. Examples:
--
--   * For @let x = Just (42, 'z')@ we want to record the
--     constraints @x ~ Just a, a ~ (b, c), b ~ 42, c ~ 'z'@.
--     See 'data_con_app'.
--   * For @let x = unpackCString# "tmp"@ we want to record the literal
--     constraint @x ~ "tmp"@.
--   * For @let x = I# 42@ we want the literal constraint @x ~ 42@. Similar
--     for other literals. See 'coreExprAsPmLit'.
--   * Finally, if we have @let x = e@ and we already have seen @let y = e@, we
--     want to record @x ~ y@.
addCoreCt :: Nabla -> Id -> CoreExpr -> MaybeT DsM Nabla
addCoreCt nabla x e = do
  simpl_opts <- initSimpleOpts <$> getDynFlags
  let e' = simpleOptExpr simpl_opts e
  -- lift $ tracePm "addCoreCt" (ppr x <+> dcolon <+> ppr (idType x) $$ ppr e $$ ppr e')
  execStateT (core_expr x e') nabla
  where
    -- | Takes apart a 'CoreExpr' and tries to extract as much information about
    -- literals and constructor applications as possible.
    core_expr :: Id -> CoreExpr -> StateT Nabla (MaybeT DsM) ()
    -- TODO: Handle newtypes properly, by wrapping the expression in a DataCon
    -- This is the right thing for casts involving data family instances and
    -- their representation TyCon, though (which are not visible in source
    -- syntax). See Note [COMPLETE sets on data families]
    -- core_expr x e | pprTrace "core_expr" (ppr x $$ ppr e) False = undefined
    core_expr x (Cast e _co) = core_expr x e
    core_expr x (Tick _t e) = core_expr x e
    core_expr x e
      | Just (pmLitAsStringLit -> Just s) <- coreExprAsPmLit e
      , expr_ty `eqType` stringTy
      -- See Note [Representation of Strings in TmState]
      = case unpackFS s of
          -- We need this special case to break a loop with coreExprAsPmLit
          -- Otherwise we alternate endlessly between [] and ""
          [] -> data_con_app x emptyInScopeSet nilDataCon []
          s' -> core_expr x (mkListExpr charTy (map mkCharExpr s'))
      | Just lit <- coreExprAsPmLit e
      = pm_lit x lit
      | Just (in_scope, _empty_floats@[], dc, _arg_tys, args)
            <- exprIsConApp_maybe in_scope_env e
      = data_con_app x in_scope dc args
      -- See Note [Detecting pattern synonym applications in expressions]
      | Var y <- e, Nothing <- isDataConId_maybe x
      -- We don't consider DataCons flexible variables
      = modifyT (\nabla -> addVarCt nabla x y)
      | otherwise
      -- Any other expression. Try to find other uses of a semantically
      -- equivalent expression and represent them by the same variable!
      = equate_with_similar_expr x e
      where
        expr_ty       = exprType e
        expr_in_scope = mkInScopeSet (exprFreeVars e)
        in_scope_env  = (expr_in_scope, const NoUnfolding)
        -- It's inconvenient to get hold of a global in-scope set
        -- here, but it'll only be needed if exprIsConApp_maybe ends
        -- up substituting inside a forall or lambda (i.e. seldom)
        -- so using exprFreeVars seems fine.   See MR !1647.

    -- | The @e@ in @let x = e@ had no familiar form. But we can still see if
    -- see if we already encountered a constraint @let y = e'@ with @e'@
    -- semantically equivalent to @e@, in which case we may add the constraint
    -- @x ~ y@.
    equate_with_similar_expr :: Id -> CoreExpr -> StateT Nabla (MaybeT DsM) ()
    equate_with_similar_expr x e = do
      rep <- StateT $ \nabla -> swap <$> lift (representCoreExpr nabla e)
      -- Note that @rep == x@ if we encountered @e@ for the first time.
      modifyT (\nabla -> addVarCt nabla x rep)

    bind_expr :: CoreExpr -> StateT Nabla (MaybeT DsM) Id
    bind_expr e = do
      x <- lift (lift (mkPmId (exprType e)))
      core_expr x e
      pure x

    -- | Look at @let x = K taus theta es@ and generate the following
    -- constraints (assuming universals were dropped from @taus@ before):
    --   1. @x /~ ⊥@ if 'K' is not a Newtype constructor.
    --   2. @a_1 ~ tau_1, ..., a_n ~ tau_n@ for fresh @a_i@
    --   3. @y_1 ~ e_1, ..., y_m ~ e_m@ for fresh @y_i@
    --   4. @x ~ K as ys@
    -- This is quite similar to PmCheck.pmConCts.
    data_con_app :: Id -> InScopeSet -> DataCon -> [CoreExpr] -> StateT Nabla (MaybeT DsM) ()
    data_con_app x in_scope dc args = do
      let dc_ex_tvs              = dataConExTyCoVars dc
          arty                   = dataConSourceArity dc
          (ex_ty_args, val_args) = splitAtList dc_ex_tvs args
          ex_tys                 = map exprToType ex_ty_args
          vis_args               = reverse $ take arty $ reverse val_args
      uniq_supply <- lift $ lift $ getUniqueSupplyM
      let (_, ex_tvs) = cloneTyVarBndrs (mkEmptyTCvSubst in_scope) dc_ex_tvs uniq_supply
          ty_cts      = equateTys (map mkTyVarTy ex_tvs) ex_tys
      -- 1. @x /~ ⊥@ if 'K' is not a Newtype constructor (#18341)
      when (not (isNewDataCon dc)) $
        modifyT $ \nabla -> addNotBotCt nabla x
      -- 2. @a_1 ~ tau_1, ..., a_n ~ tau_n@ for fresh @a_i@. See also #17703
      modifyT $ \nabla -> MaybeT $ addPmCts nabla (listToBag ty_cts)
      -- 3. @y_1 ~ e_1, ..., y_m ~ e_m@ for fresh @y_i@
      arg_ids <- traverse bind_expr vis_args
      -- 4. @x ~ K as ys@
      pm_alt_con_app x (PmAltConLike (RealDataCon dc)) ex_tvs arg_ids

    -- | Adds a literal constraint, i.e. @x ~ 42@.
    -- Also we assume that literal expressions won't diverge, so this
    -- will add a @x ~/ ⊥@ constraint.
    pm_lit :: Id -> PmLit -> StateT Nabla (MaybeT DsM) ()
    pm_lit x lit = do
      modifyT $ \nabla -> addNotBotCt nabla x
      pm_alt_con_app x (PmAltLit lit) [] []

    -- | Adds the given constructor application as a solution for @x@.
    pm_alt_con_app :: Id -> PmAltCon -> [TyVar] -> [Id] -> StateT Nabla (MaybeT DsM) ()
    pm_alt_con_app x con tvs args = modifyT $ \nabla -> addConCt nabla x con tvs args

-- | Like 'modify', but with an effectful modifier action
modifyT :: Monad m => (s -> m s) -> StateT s m ()
modifyT f = StateT $ fmap ((,) ()) . f

{- Note [Detecting pattern synonym applications in expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At the moment we fail to detect pattern synonyms in scrutinees and RHS of
guards. This could be alleviated with considerable effort and complexity, but
the returns are meager. Consider:

    pattern P
    pattern Q
    case P 15 of
      Q _  -> ...
      P 15 ->

Compared to the situation where P and Q are DataCons, the lack of generativity
means we could never flag Q as redundant. (also see Note [Undecidable Equality
for PmAltCons] in PmTypes.) On the other hand, if we fail to recognise the
pattern synonym, we flag the pattern match as inexhaustive. That wouldn't happen
if we had knowledge about the scrutinee, in which case the oracle basically
knows "If it's a P, then its field is 15".

This is a pretty narrow use case and I don't think we should to try to fix it
until a user complains energetically.
-}
