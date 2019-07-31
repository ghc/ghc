{-
Author: George Karachalias <george.karachalias@cs.kuleuven.be>
-}

{-# LANGUAGE CPP, LambdaCase, TupleSections, PatternSynonyms, ViewPatterns #-}

-- | The term equality oracle. The main export of the module are the functions
-- 'pmOracle', 'solveOneEq' and 'tryAddRefutableAltCon'.
--
-- If you are looking for an oracle that can solve type-level constraints, look
-- at 'TcSimplify.tcCheckSatisfiability'.
module PmOracle (

        PmM, tracePm, mkPmId,

        Delta, pmInitialTmTyCs, canDiverge,

        inhabitants,
        tryAddRefutableAltCon, -- Add a negative equality
        refineToAltCon,        -- Add a positive equality x ~ K _ _
        solveVar,              -- Add a positive equality x ~ e

        -- misc.
        wrapUpRefutableShapes, exprDeepLookup
    ) where

#include "HsVersions.h"

import GhcPrelude

import PmExpr

import DynFlags
import Outputable
import ErrUtils
import BasicTypes
import Util
import Bag
import UniqSet
import Id
import VarEnv
import Var           (EvVar)
import Name
import UniqSupply
import FastString
import SrcLoc
import ListSetOps (unionLists)
import Maybes
import ConLike
import DataCon
import TyCon
import TysWiredIn
import TysPrim (tYPETyCon)
import TyCoRep
import Type
import TcSimplify    (tcNormalise, tcCheckSatisfiability)
import Unify         (tcMatchTy)
import TcRnTypes     (pprEvVarWithType, completeMatchConLikes)
import Coercion
import MonadUtils hiding (foldlM)
import DsMonad hiding (foldlM)
import Control.Monad (zipWithM, mzero)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (foldlM)
import Data.List     (find)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import FamInstEnv

type PmM = DsM

-- Debugging Infrastructre

tracePm :: String -> SDoc -> PmM ()
tracePm herald doc = do
  dflags <- getDynFlags
  printer <- mkPrintUnqualifiedDs
  liftIO $ dumpIfSet_dyn_printer printer dflags
            Opt_D_dump_ec_trace (text herald $$ (nest 2 doc))

-- | Generate a fresh `Id` of a given type
mkPmId :: Type -> PmM Id
mkPmId ty = getUniqueM >>= \unique ->
  let occname = mkVarOccFS $ fsLit "$pm"
      name    = mkInternalName unique occname noSrcSpan
  in  return (mkLocalId name ty)

-- | Introduce a new 'Id' that has the given type and is in the same equivalence
-- class as the argument.
mkIdCoercion :: Id -> Type -> Delta -> PmM (Id, Delta)
mkIdCoercion x ty delta
  | eqType (idType x) ty = pure (x, delta) -- no need to introduce anything new
  | otherwise = do
      y <- mkPmId ty
      let e = PmExprVar x
      pure (y, delta { delta_tm_cs = extendSubst y e (delta_tm_cs delta) })

type ConLikeSet = UniqSet ConLike -- TODO: UniqDSet?

newtype IncompleteMatches = IM (NonEmpty ConLikeSet)
  -- Each ConLikeSet is a (subset of) the constructors in a COMPLETE pragma
  --
  -- NonEmpty because the empty case would mean that there
  --   are no matching constructors at all

instance Outputable IncompleteMatches where
  ppr (IM cs) = ppr (NonEmpty.toList cs)

initIM :: Type -> DsM (Maybe IncompleteMatches)
initIM ty = case splitTyConApp_maybe ty of
  Nothing -> pure Nothing
  Just (tc, _) -> do
    let mb_rdcs = map RealDataCon <$> tyConDataCons_maybe tc
    let maybe_to_list = maybe [] (:[])
    let rdcs = maybe_to_list mb_rdcs
    pragmas <- dsGetCompleteMatches tc
    let fams = mapM dsLookupConLike . completeMatchConLikes
    pscs <- mapM fams pragmas
    pure (IM . fmap mkUniqSet <$> NonEmpty.nonEmpty (rdcs ++ pscs))

markMatched :: ConLike -> IncompleteMatches -> IncompleteMatches
markMatched con (IM ms) = IM (fmap (`delOneFromUniqSet` con) ms)

ensureInhabited :: Monad m
                => (ConLike -> m Bool)   -- Oracle: True <=> this ConLike is inhabited
                -> IncompleteMatches
                -> m (Maybe IncompleteMatches)
   -- Returns (Just im) guarantees that at least one member
   -- of each ConLikeSet satisfies the oracle
   --
   -- NB: Does /not/ filter each ConLikeSet with the oracle; members may
   --     remain that do not statisfy it.  This lazy approach just
   --     avoids doing unnecessary work.
ensureInhabited inh_test (IM ms) = runMaybeT (IM <$> traverse one_set ms)
  where
    one_set cs = find_one_inh cs (nonDetEltsUniqSet cs)

    find_one_inh :: ConLikeSet -> [ConLike] -> MaybeT m ConLikeSet
    -- (find_one_inh cs cls) iterates over cls, deleting from cs
    -- any uninhabited elements of cls.  Stop (returning Just cs)
    -- when you see an inhabited element; return Nothing if all
    -- are uninhabited
    find_one_inh _  [] = mzero
    find_one_inh cs (con:cons) = lift (inh_test con) >>= \case
      True  -> pure cs
      False -> find_one_inh (delOneFromUniqSet cs con) cons

-- -----------------------------------------------------------------------
-- * Types and constraints

newEvVar :: Name -> Type -> EvVar
newEvVar name ty = mkLocalId name ty

nameType :: String -> Type -> DsM EvVar
nameType name ty = do
  unique <- getUniqueM
  let occname = mkVarOccFS (fsLit (name++"_"++show unique))
      idname  = mkInternalName unique occname noSrcSpan
  return (newEvVar idname ty)

{- *********************************************************************
*                                                                      *
*         The Delta data type: what the oracle knows                   *
*                                                                      *
********************************************************************* -}


-- | Term and type constraints to accompany each value vector abstraction.
-- For efficiency, we store the term oracle state instead of the term
-- constraints. TODO: Do the same for the type constraints?
data Delta = MkDelta { delta_ty_cs :: Bag EvVar  -- Type oracle; things like a~Int
                     , delta_tm_cs :: TmState }  -- Term oracle; things like x~Nothing

instance Outputable Delta where
  ppr delta = hcat [
      -- intentionally formatted this way enable the dev to comment in only
      -- the info she needs
      ppr (delta_tm_cs delta),
      ppr (pprEvVarWithType <$> delta_ty_cs delta)
      --ppr (delta_ty_cs delta)
    ]

-- | Determine suitable constraints to use at the beginning of pattern-match
-- coverage checking by consulting the sets of term and type constraints
-- currently in scope. If one of these sets of constraints is unsatisfiable,
-- use an empty set in its place. (See
-- @Note [Recovering from unsatisfiable pattern-matching constraints]@
-- for why this is done.)
pmInitialTmTyCs :: PmM Delta
pmInitialTmTyCs = do
  ty_cs  <- getDictsDs
  tm_cs  <- bagToList <$> getTmCsDs
  sat_ty <- tyOracle ty_cs
  let initTyCs = if sat_ty then ty_cs else emptyBag
      initTmState = fromMaybe initialTmState (pmOracle initialTmState tm_cs)
  pure $ MkDelta initTyCs initTmState

-- | Information about a conlike that is relevant to coverage checking.
-- It is called an \"inhabitation candidate\" since it is a value which may
-- possibly inhabit some type, but only if its term constraints ('ic_tm_cs')
-- and type constraints ('ic_ty_cs') are permitting, and if all of its strict
-- argument types ('ic_strict_arg_tys') are inhabitable.
-- See @Note [Extensions to GADTs Meet Their Match]@.
data InhabitationCandidate =
  InhabitationCandidate
  { ic_tm_cs          :: Bag TmVarCt
  , ic_ty_cs          :: Bag EvVar
  , ic_strict_arg_tys :: [Type]
  }

instance Outputable InhabitationCandidate where
  ppr (InhabitationCandidate tm_cs ty_cs strict_arg_tys) =
    text "InhabitationCandidate" <+>
      vcat [ text "ic_tm_cs          =" <+> ppr tm_cs
           , text "ic_ty_cs          =" <+> ppr ty_cs
           , text "ic_strict_arg_tys =" <+> ppr strict_arg_tys ]

-- | Like 'pmIsSatisfiable', but only checks if term and type constraints are
-- satisfiable, and doesn't bother checking anything related to strict argument
-- types.
tmTyCsAreSatisfiable
  :: Delta       -- ^ The ambient term and type constraints
                 --   (known to be satisfiable).
  -> Bag TmVarCt -- ^ The new term constraint.
  -> Bag EvVar   -- ^ The new type constraints.
  -> PmM (Maybe Delta)
       -- ^ @'Just' delta@ if the constraints (@delta@) are
       -- satisfiable. 'Nothing' otherwise.
tmTyCsAreSatisfiable
    (MkDelta{ delta_tm_cs = amb_tm_cs, delta_ty_cs = amb_ty_cs })
    new_tm_cs new_ty_cs = do
  let ty_cs = new_ty_cs `unionBags` amb_ty_cs
  sat_ty <- if isEmptyBag new_ty_cs
               then pure True
               else tyOracle ty_cs
  pure $ case (sat_ty, pmOracle amb_tm_cs new_tm_cs) of
           (True, Just term_cs) -> ensureAllPossibleSetsInhabited (MkDelta ty_cs term_cs)
           _unsat               -> Nothing

ensureAllPossibleSetsInhabited :: Delta -> Maybe Delta
ensureAllPossibleSetsInhabited delta@MkDelta{ delta_tm_cs = TS env } = delta{ delta_tm_cs = ts' }
  where
    ts' = TS <$> traverseDVarEnv go env
    traverseDVarEnv f
      = fmap listToUDFM_Directly
      . traverse (\(u,a) -> (u,) <$> f a)
      . udfmToList
    go (VI ty (CompleteSets im)) = undefined
      -- | any new constraints influence ty = do stuff
      -- But how to call mkOneConFull anyway?! Needs the Id... so another
      -- refactor incoming. Proabably should attach the Id to CompleteSets
      -- instead anyway.

-- | Given a conlike's term constraints, type constraints, and strict argument
-- types, check if they are satisfiable.
-- (In other words, this is the ⊢_Sat oracle judgment from the GADTs Meet
-- Their Match paper.)
--
-- For the purposes of efficiency, this takes as separate arguments the
-- ambient term and type constraints (which are known beforehand to be
-- satisfiable), as well as the new term and type constraints (which may not
-- be satisfiable). This lets us implement two mini-optimizations:
--
-- * If there are no new type constraints, then don't bother initializing
--   the type oracle, since it's redundant to do so.
-- * Since the new term constraint is a separate argument, we only need to
--   execute one iteration of the term oracle (instead of traversing the
--   entire set of term constraints).
--
-- Taking strict argument types into account is something which was not
-- discussed in GADTs Meet Their Match. For an explanation of what role they
-- serve, see @Note [Extensions to GADTs Meet Their Match]@.
pmIsSatisfiable
  :: Delta       -- ^ The ambient term and type constraints
                 --   (known to be satisfiable).
  -> Bag TmVarCt -- ^ The new term constraints.
  -> Bag EvVar   -- ^ The new type constraints.
  -> [Type]      -- ^ The strict argument types.
  -> PmM (Maybe Delta)
                 -- ^ @'Just' delta@ if the constraints (@delta@) are
                 -- satisfiable, and each strict argument type is inhabitable.
                 -- 'Nothing' otherwise.
pmIsSatisfiable amb_cs new_tm_c new_ty_cs strict_arg_tys = do
  mb_sat <- tmTyCsAreSatisfiable amb_cs new_tm_c new_ty_cs
  case mb_sat of
    Nothing -> pure Nothing
    Just delta -> do
      -- We know that the term and type constraints are inhabitable, so now
      -- check if each strict argument type is inhabitable.
      all_non_void <- checkAllNonVoid initRecTc delta strict_arg_tys
      pure $ if all_non_void -- Check if each strict argument type
                             -- is inhabitable
                then Just delta
                else Nothing

-- | Implements two performance optimizations, as described in the
-- \"Strict argument type constraints\" section of
-- @Note [Extensions to GADTs Meet Their Match]@.
checkAllNonVoid :: RecTcChecker -> Delta -> [Type] -> PmM Bool
checkAllNonVoid rec_ts amb_cs strict_arg_tys = do
  fam_insts <- dsGetFamInstEnvs
  let definitely_inhabited =
        definitelyInhabitedType fam_insts (delta_ty_cs amb_cs)
  tys_to_check <- filterOutM definitely_inhabited strict_arg_tys
  let rec_max_bound | tys_to_check `lengthExceeds` 1
                    = 1
                    | otherwise
                    = defaultRecTcMaxBound
      rec_ts' = setRecTcMaxBound rec_max_bound rec_ts
  allM (nonVoid rec_ts' amb_cs) tys_to_check

-- | Checks if a strict argument type of a conlike is inhabitable by a
-- terminating value (i.e, an 'InhabitationCandidate').
-- See @Note [Extensions to GADTs Meet Their Match]@.
nonVoid
  :: RecTcChecker -- ^ The per-'TyCon' recursion depth limit.
  -> Delta        -- ^ The ambient term/type constraints (known to be
                  --   satisfiable).
  -> Type         -- ^ The strict argument type.
  -> PmM Bool     -- ^ 'True' if the strict argument type might be inhabited by
                  --   a terminating value (i.e., an 'InhabitationCandidate').
                  --   'False' if it is definitely uninhabitable by anything
                  --   (except bottom).
nonVoid rec_ts amb_cs strict_arg_ty = do
  mb_cands <- inhabitationCandidates amb_cs strict_arg_ty
  case mb_cands of
    Right (tc, _, cands)
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
    -- See Note [Extensions to GADTs Meet Their Match]
    cand_is_inhabitable :: RecTcChecker -> Delta
                        -> InhabitationCandidate -> PmM Bool
    cand_is_inhabitable rec_ts amb_cs
      (InhabitationCandidate{ ic_tm_cs          = new_tm_cs
                            , ic_ty_cs          = new_ty_cs
                            , ic_strict_arg_tys = new_strict_arg_tys }) = do
        mb_sat <- tmTyCsAreSatisfiable amb_cs new_tm_cs new_ty_cs
        case mb_sat of
          Nothing -> pure False
          Just new_delta ->
            checkAllNonVoid rec_ts new_delta new_strict_arg_tys

-- | @'definitelyInhabitedType' ty@ returns 'True' if @ty@ has at least one
-- constructor @C@ such that:
--
-- 1. @C@ has no equality constraints.
-- 2. @C@ has no strict argument types.
--
-- See the \"Strict argument type constraints\" section of
-- @Note [Extensions to GADTs Meet Their Match]@.
definitelyInhabitedType :: FamInstEnvs -> Bag EvVar -> Type -> PmM Bool
definitelyInhabitedType env ty_cs ty = do
  mb_res <- pmTopNormaliseType_maybe env ty_cs ty
  pure $ case mb_res of
           Just (_, cons, _) -> any meets_criteria cons
           Nothing           -> False
  where
    meets_criteria :: DataCon -> Bool
    meets_criteria con =
      null (dataConEqSpec con) && -- (1)
      null (dataConImplBangs con) -- (2)

{- Note [Type normalisation for EmptyCase]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
EmptyCase is an exception for pattern matching, since it is strict. This means
that it boils down to checking whether the type of the scrutinee is inhabited.
Function pmTopNormaliseType_maybe gets rid of the outermost type function/data
family redex and newtypes, in search of an algebraic type constructor, which is
easier to check for inhabitation.

It returns 3 results instead of one, because there are 2 subtle points:
1. Newtypes are isomorphic to the underlying type in core but not in the source
   language,
2. The representational data family tycon is used internally but should not be
   shown to the user

Hence, if pmTopNormaliseType_maybe env ty_cs ty = Just (src_ty, dcs, core_ty),
then
  (a) src_ty is the rewritten type which we can show to the user. That is, the
      type we get if we rewrite type families but not data families or
      newtypes.
  (b) dcs is the list of data constructors "skipped", every time we normalise a
      newtype to its core representation, we keep track of the source data
      constructor.
  (c) core_ty is the rewritten type. That is,
        pmTopNormaliseType_maybe env ty_cs ty = Just (src_ty, dcs, core_ty)
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

In this case pmTopNormaliseType_maybe env ty_cs (F Int) results in

  Just (G2, [MkG2,MkG1], R:TInt)

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
pmTopNormaliseType_maybe, using the constraint solver to solve for any local
equalities (such as i ~ Int) that may be in scope.
-}

pmTopNormaliseType_maybe :: FamInstEnvs -> Bag EvVar -> Type
                         -> PmM (Maybe (Type, [DataCon], Type))
-- ^ Get rid of *outermost* (or toplevel)
--      * type function redex
--      * data family redex
--      * newtypes
--
-- Behaves exactly like `topNormaliseType_maybe`, but instead of returning a
-- coercion, it returns useful information for issuing pattern matching
-- warnings. See Note [Type normalisation for EmptyCase] for details.
--
-- NB: Normalisation can potentially change kinds, if the head of the type
-- is a type family with a variable result kind. I (Richard E) can't think
-- of a way to cause trouble here, though.
pmTopNormaliseType_maybe env ty_cs typ
  = do (_, mb_typ') <- initTcDsForSolver $ tcNormalise ty_cs typ
         -- Before proceeding, we chuck typ into the constraint solver, in case
         -- solving for given equalities may reduce typ some. See
         -- "Wrinkle: local equalities" in
         -- Note [Type normalisation for EmptyCase].
       pure $ do typ' <- mb_typ'
                 ((ty_f,tm_f), ty) <- topNormaliseTypeX stepper comb typ'
                 -- We need to do topNormaliseTypeX in addition to tcNormalise,
                 -- since topNormaliseX looks through newtypes, which
                 -- tcNormalise does not do.
                 Just (eq_src_ty ty (typ' : ty_f [ty]), tm_f [], ty)
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

    stepper = newTypeStepper `composeSteppers` tyFamStepper

    -- A 'NormaliseStepper' that unwraps newtypes, careful not to fall into
    -- a loop. If it would fall into a loop, it produces 'NS_Abort'.
    newTypeStepper :: NormaliseStepper ([Type] -> [Type],[DataCon] -> [DataCon])
    newTypeStepper rec_nts tc tys
      | Just (ty', _co) <- instNewTyCon_maybe tc tys
      = case checkRecTc rec_nts tc of
          Just rec_nts' -> let tyf = ((TyConApp tc tys):)
                               tmf = ((tyConSingleDataCon tc):)
                           in  NS_Step rec_nts' ty' (tyf, tmf)
          Nothing       -> NS_Abort
      | otherwise
      = NS_Done

    tyFamStepper :: NormaliseStepper ([Type] -> [Type], [DataCon] -> [DataCon])
    tyFamStepper rec_nts tc tys  -- Try to step a type/data family
      = let (_args_co, ntys, _res_co) = normaliseTcArgs env Representational tc tys in
          -- NB: It's OK to use normaliseTcArgs here instead of
          -- normalise_tc_args (which takes the LiftingContext described
          -- in Note [Normalising types]) because the reduceTyFamApp below
          -- works only at top level. We'll never recur in this function
          -- after reducing the kind of a bound tyvar.

        case reduceTyFamApp_maybe env Representational tc ntys of
          Just (_co, rhs) -> NS_Step rec_nts rhs ((rhs:), id)
          _               -> NS_Done

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

-- | Generate an 'InhabitationCandidate' for a given 'ConLike'.
-- Generate fresh variables of the appropriate type for arguments and return
-- them alongside.
mkOneConFull :: Id -> ConLike -> PmM (InhabitationCandidate, [TyVar], [Id])
--  *  x :: T tys, where T is an algebraic data type
--     NB: in the case of a data family, T is the *representation* TyCon
--     e.g.   data instance T (a,b) = T1 a b
--       leads to
--            data TPair a b = T1 a b  -- The "representation" type
--       It is TPair, not T, that is given to mkOneConFull
--
--  * 'con' K is a conlike of data type T
--
-- After instantiating the universal tyvars of K we get
--          K tys :: forall e1 .. en. Q => s1 .. sn -> T tys
--
-- Suppose y1 is a strict field. Then we get
-- Results: ic_tm_cs:          x ~ K (y1::s1) .. (yn::sn)
--          ic_ty_cs:          Q
--          ic_strict_arg_tys: [s1]
--          [e1,..,en]
--          [y1,..,yn]
mkOneConFull x con = do
  let res_ty  = idType x
      (univ_tvs, ex_tvs, eq_spec, thetas, _req_theta , arg_tys, con_res_ty)
        = conLikeFullSig con
      arg_is_banged = map isBanged $ conLikeImplBangs con
      -- tyConAppArgs crashes for T11336(b), so use splitAppTy instead. Should
      -- be fine after type-checking.
      (_, tc_args) = splitAppTys res_ty
  let subst1 = case con of
                  RealDataCon {} -> zipTvSubst univ_tvs tc_args
                  -- The expectJust is always satisfied as long as we filter
                  -- with 'isValidCompleteMatch' in 'allCompleteMatches'
                  PatSynCon {}   -> expectJust "mkOneConFull" (tcMatchTy con_res_ty res_ty)
                                    -- See Note [Pattern synonym result type] in PatSyn

  (subst, ex_tvs') <- cloneTyVarBndrs subst1 ex_tvs <$> getUniqueSupplyM

  let arg_tys' = substTys subst arg_tys
  -- Fresh term variables (VAs) as arguments to the constructor
  vars <- mapM mkPmId arg_tys'
  let args = map PmExprVar vars
  -- All constraints bound by the constructor (alpha-renamed)
  let theta_cs = substTheta subst (eqSpecPreds eq_spec ++ thetas)
  evvars <- mapM (nameType "pm") theta_cs
  let expr = PmExprCon (PmAltConLike con) args
      strict_arg_tys = filterByList arg_is_banged arg_tys'
  let ic = InhabitationCandidate
           { ic_tm_cs          = unitBag (TVC x expr)
           , ic_ty_cs          = listToBag evvars
           , ic_strict_arg_tys = strict_arg_tys
           }
  return (ic, ex_tvs', vars)

-- | Invoke 'mkOneConFull' and immediately check whether the resulting
-- 'InhabitationCandidate' @ic@ with arguments @arg_vars@ is inhabited by
-- consulting 'pmIsSatisfiable'. Return @Just (new_delta, ic, ex_tvs, arg_vars)@
-- if it is.
--
-- Suppose K :: forall a b x y. (x,b) -> y -> T a b
--   where the x,y are the existentials
-- (mkOneSatifiableConFull delta x K) extends delta with the
--   positive information x :-> K x' y' p q, for some fresh x', y', p, q.
--   Return the fresh [x',y'] existentials and [p,q] args
-- Return Nothing if such a match is contradictory with delta.
mkOneSatisfiableConFull
  :: Delta -> Id -> ConLike
  -> PmM (Maybe (Delta, [TyVar], [Id]))
mkOneSatisfiableConFull delta x con = do
  -- mkOneConFull doesn't cope with type families, so we have to normalise
  -- x's result type first and introduce an auxiliary binding.
  fam_insts <- dsGetFamInstEnvs
  mb_res_ty <- pmTopNormaliseType_maybe fam_insts (delta_ty_cs delta) (idType x)
  let res_ty = maybe (idType x) fstOf3 mb_res_ty
  (y, delta') <- mkIdCoercion x res_ty delta
  tracePm "coercing" (ppr x $$ ppr (idType x) $$ ppr y $$ ppr res_ty)
  (_ic, ex_tvs, arg_vars) <- mkOneConFull y con
  mb_delta <- pmIsSatisfiable delta' (ic_tm_cs ic) (ic_ty_cs ic) (ic_strict_arg_tys ic)
  tracePm "mkOneSatisfiableConFull" (ppr x <+> ppr y $$ ppr ic $$ ppr delta' $$ ppr mb_delta)
  pure ((,ex_tvs,arg_vars) <$> mb_delta)

-- | Generate all 'InhabitationCandidate's for a given type. The result is
-- either @'Left' ty@, if the type cannot be reduced to a closed algebraic type
-- (or if it's one trivially inhabited, like 'Int'), or @'Right' candidates@,
-- if it can. In this case, the candidates are the signature of the tycon, each
-- one accompanied by the term- and type- constraints it gives rise to.
-- See also Note [Checking EmptyCase Expressions]
inhabitationCandidates :: Delta -> Type
                       -> PmM (Either Type (TyCon, Id, [InhabitationCandidate]))
inhabitationCandidates MkDelta{ delta_ty_cs = ty_cs } ty = do
  fam_insts   <- dsGetFamInstEnvs
  pmTopNormaliseType_maybe fam_insts ty_cs ty >>= \case
    Just (src_ty, dcs, core_ty) -> alts_to_check src_ty core_ty dcs
    Nothing                     -> alts_to_check ty     ty      []
  where
    -- All these types are trivially inhabited
    trivially_inhabited = [ charTyCon, doubleTyCon, floatTyCon
                          , intTyCon, wordTyCon, word8TyCon ]

    build_tm :: PmExpr -> [DataCon] -> PmExpr
    build_tm = foldr (\dc e -> mkPmExprData dc [e])

    -- Inhabitation candidates, using the result of pmTopNormaliseType_maybe
    alts_to_check :: Type -> Type -> [DataCon]
                  -> PmM (Either Type (TyCon, Id, [InhabitationCandidate]))
    alts_to_check src_ty core_ty dcs = case splitTyConApp_maybe core_ty of
      Just (tc, _)
        |  tc `elem` trivially_inhabited
        -> case dcs of
             []    -> return (Left src_ty)
             (_:_) -> do inner <- mkPmId core_ty
                         let expr = build_tm (PmExprVar inner) dcs
                         outer <- mkPmId src_ty
                         return $ Right (tc, outer, [InhabitationCandidate
                           { ic_tm_cs = unitBag (TVC outer expr)
                           , ic_ty_cs = emptyBag, ic_strict_arg_tys = [] }])

        |  pmIsClosedType core_ty && not (isAbstractTyCon tc)
           -- Don't consider abstract tycons since we don't know what their
           -- constructors are, which makes the results of coverage checking
           -- them extremely misleading.
        -> do
             inner <- mkPmId core_ty -- it would be wrong to unify inner
             alts <- mapM (fmap fstOf3 . mkOneConFull inner . RealDataCon) (tyConDataCons tc)
             outer <- mkPmId src_ty
             let new_tm_ct = TVC outer (build_tm (PmExprVar inner) dcs)
             let wrap_dcs alt = alt{ ic_tm_cs = new_tm_ct `consBag` ic_tm_cs alt}
             return $ Right (tc, outer, map wrap_dcs alts)
      -- For other types conservatively assume that they are inhabited.
      _other -> return (Left src_ty)

inhabitants :: Delta -> Type -> PmM (Either Type (Id, [Delta]))
inhabitants delta ty = inhabitationCandidates delta ty >>= \case
  Left ty' -> pure (Left ty')
  Right (_, va, candidates) -> do
    deltas <- flip mapMaybeM candidates $
        \InhabitationCandidate{ ic_tm_cs = tm_cs
                              , ic_ty_cs = ty_cs
                              , ic_strict_arg_tys = strict_arg_tys } -> do
      pmIsSatisfiable delta tm_cs ty_cs strict_arg_tys
    pure (Right (va, deltas))

{- Note [Checking EmptyCase Expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Empty case expressions are strict on the scrutinee. That is, `case x of {}`
will force argument `x`. Hence, `checkMatches` is not sufficient for checking
empty cases, because it assumes that the match is not strict (which is true
for all other cases, apart from EmptyCase). This gave rise to #10746. Instead,
we do the following:

1. We normalise the outermost type family redex, data family redex or newtype,
   using pmTopNormaliseType_maybe (in types/FamInstEnv.hs). This computes 3
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

   For an example see also Note [Type normalisation for EmptyCase]
   in types/FamInstEnv.hs.

2. Function Check.checkEmptyCase' performs the check:
   - If core_ty is not an algebraic type, then we cannot check for
     inhabitation, so we emit (_ :: src_ty) as missing, conservatively assuming
     that the type is inhabited.
   - If core_ty is an algebraic type, then we unfold the scrutinee to all
     possible constructor patterns, using inhabitationCandidates, and then
     check each one for constraint satisfiability, same as we do for normal
     pattern match checking.
-}

{-
%************************************************************************
%*                                                                      *
                              The type oracle
%*                                                                      *
%************************************************************************
-}

-- | Check whether a set of type constraints is satisfiable.
tyOracle :: Bag EvVar -> PmM Bool
tyOracle evs
  = do { ((_warns, errs), res) <- initTcDsForSolver $ tcCheckSatisfiability evs
       ; case res of
            Just sat -> return sat
            Nothing  -> pprPanic "tyOracle" (vcat $ pprErrMsgBagWithLoc errs) }

{-
%************************************************************************
%*                                                                      *
                      The term equality oracle
%*                                                                      *
%************************************************************************
-}
{-
-- | Pretty much a @['TmVarCt']@ association list. This is the type of 'tm_pos',
-- where we store solutions for rigid pattern match variables.
type TmVarCtEnv = IdEnv PmExpr

-- | An environment assigning shapes to variables that immediately lead to a
-- refutation. So, if this maps @x :-> [Just]@, then trying to solve a
-- 'TmVarCt' like @x ~ Just False@ immediately leads to a contradiction.
-- Additionally, this stores the 'Type' from which to draw 'ConLike's from.
--
-- Determinism is important since we use this for warning messages in
-- 'PmPpr.pprUncovered'. We don't do the same for 'TmVarCtEnv', so that is a plain
-- 'IdEnv'.
--
-- See also Note [Refutable shapes] in PmOracle.
type PmRefutEnv = DIdEnv [PmAltCon]
-}
{-
-- | The state of the term oracle. Tracks all term-level facts of the form "x is
-- @True@" ('tm_pos') and "x is not @5@" ('tm_neg').
--
-- Subject to Note [The Pos/Neg invariant].
data TmState = TmS
  { tm_pos :: !TmVarCtEnv
  -- ^ A substitution with solutions we extend with every step and return as a
  -- result. The substitution is in /triangular form/: It might map @x@ to @y@
  -- where @y@ itself occurs in the domain of 'tm_pos', rendering lookup
  -- non-idempotent. This means that 'varDeepLookup' potentially has to walk
  -- along a chain of var-to-var mappings until we find the solution but has the
  -- advantage that when we update the solution for @y@ above, we automatically
  -- update the solution for @x@ in a union-find-like fashion.
  , tm_neg :: !PmRefutEnv
  -- ^ Maps each variable @x@ to a list of 'PmAltCon's that @x@ definitely
  -- cannot match. Example, assuming
  --
  -- @
  --     data T = Leaf Int | Branch T T | Node Int T
  -- @
  --
  -- then @x :-> [Leaf, Node]@ means that @x@ cannot match a @Leaf@ or @Node@,
  -- and hence can only match @Branch@. Should we later 'equate' @x@ to a
  -- variable @y@, we merge the refutable shapes of @x@ into those of @y@. See
  -- also Note [The Pos/Neg invariant].
  }
-}

newtype TmState = TS (DIdEnv VarInfo)
  -- Deterministic so that we generate deterministic error messages

data VarInfo
  = VI
  { vi_ty  :: !Type           -- the type of what 'vi_pos' and 'vi_neg' refer to
  , vi_pos :: !PossibleShape  -- Positive info: things it could be
  , vi_neg :: ![PmAltCon]     -- Negative info: it is not headed by these AltCons
      -- Invariant: vi_pos and vi_neg never contradict each other
  }

data PossibleShape
  = Rigid !PmExpr
      -- Get this in the "taken" branch of a pattern match;
      --   e.g. f x@(Just y) = <rhs>
      --   In <rhs> we have x ~ Just y
      -- The PmExpr is: a data con application, literal, or variable

  | CompleteSets TyCon !IncompleteMatches

  | NoInfoYet    -- The type of the variable is not (yet) a data type

{- Note [The Pos/Neg invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Invariant: In any 'TmState', whenever there is @x ~ C@ in 'tm_pos',
an entry @x :-> cs@ in 'tm_neg' may only have incomparable 'PmAltCons' according
to 'decEqPmAltCons'.

For example, it would make no sense to say both
    tm_pos = [...x :-> 3...]
    tm_neg = [...x :-> [4,42]...]
The positive information is strictly more informative than the negative.
On the other hand
    tm_pos = [...x :-> I# y...]
    tm_neg = [...x :-> [4]...]
We want to know that @x@ is certainly not the literal 4 when we know it is a
@I#@. Notice that @PmAltLit 4@ and @PmAltConLike I#@ are incomparable. In
general, we consider every binding in 'tm_neg' informative when the equality
relation to the solution is undecidable ('decEqPmAltCons').

Now, suppose we are adding the (positive) fact @x :-> e@ to 'tm_pos'. Then we
must delete any comparable negative facts (after considering them for
refutation) for @x@ from 'tm_neg', to uphold the invariant.

But there is more! Suppose we are adding @x :-> y@ to 'tm_pos', and 'tm_neg'
contains @x :-> cs, y :-> ds@. Then we want to update 'tm_neg' to
@y :-> (cs ++ ds)@, to make use of the negative information we have about @x@,
while we can *completely* discard the entry for @x@ in 'tm_neg'.
-}

-- | Not user-facing.
instance Outputable TmState where
  ppr (TS state) = ppr state

-- | Not user-facing.
instance Outputable VarInfo where
  ppr (VI _ty pos neg) = if null neg then pos_pp else braces (neg_pp <> char ',' <+> pos_pp)
    where
      neg_pp = char '¬' <> ppr neg
      pos_pp = ppr pos

-- | Not user-facing.
instance Outputable PossibleShape where
  ppr NoInfoYet = char '?'
  ppr (Rigid sol) = ppr sol
  ppr (CompleteSets sets) = ppr sets

emptyVarInfo :: Id -> VarInfo
emptyVarInfo x = VI (idType x) NoInfoYet []

-- | Initial state of the oracle.
initialTmState :: TmState
initialTmState = TS emptyDVarEnv

lookupVarInfo :: TmState -> Id -> (Id, VarInfo)
lookupVarInfo ts@(TS env) x = case lookupDVarEnv env x of
  Just (VI _ (Rigid (PmExprVar y)) _) -> lookupVarInfo ts y
  mb_vi                               -> (x, fromMaybe (emptyVarInfo x) mb_vi)

-- | Check whether a constraint (x ~ BOT) can succeed,
-- given the resulting state of the term oracle.
canDiverge :: Delta -> Id -> Bool
canDiverge MkDelta{ delta_tm_cs = ts } x
  -- If the variable seems not evaluated, there is a possibility for
  -- constraint x ~ BOT to be satisfiable. That's the case when we haven't found
  -- a solution (i.e. some equivalent literal or constructor) for it yet.
  -- Even if we don't have a solution yet, it might be involved in a negative
  -- constraint, in which case we must already have evaluated it earlier.
  | VI _ pos [] <- snd (lookupVarInfo ts x), not_solved_yet pos
  = True
  -- Variable x is already in WHNF or we know some refutable shape, so the
  -- constraint is non-satisfiable
  | otherwise = False
  where
    not_solved_yet (Rigid PmExprCon{}) = False
    not_solved_yet _                   = True

-- | Check whether the equality @x ~ e@ leads to a refutation. Make sure that
-- @x@ and @e@ are completely substituted before!
isRefutable :: Id -> PmExpr -> TmState -> Bool
isRefutable x e ts = fromMaybe False $ do
  alt <- exprToAlt e
  let ncons = vi_neg (snd (lookupVarInfo ts x))
  pure (notNull (filter ((== Just True) . decEqPmAltCon alt) ncons))

-- | Solve an equality (top-level).
solveOneEq :: TmState -> TmVarCt -> Maybe TmState
solveOneEq solver_env (TVC x e) = unify solver_env (PmExprVar x, e)

exprToAlt :: PmExpr -> Maybe PmAltCon
exprToAlt (PmExprCon c _) = Just c
exprToAlt _               = Nothing

-- This is the only actual 'DsM' side-effect in the entire module
initIncompleteMatches :: Type -> PossibleShape -> DsM PossibleShape
initIncompleteMatches ty NoInfoYet = initIM ty >>= \case
  Nothing -> pure NoInfoYet
  Just im -> pure (CompleteSets im)
initIncompleteMatches _  pos       = pure pos

refineToAltCon :: Delta -> Id -> PmAltCon -> [TyVar] -> PmM (Maybe (Delta, [Id]))
refineToAltCon delta x l@PmAltLit{}       _ex_tvs1 =
  pure ((, []) <$> solveVar delta x (PmExprCon l []))
refineToAltCon delta x (PmAltConLike con) ex_tvs1  =
  mkOneSatisfiableConFull delta x con >>= \case
    Nothing                         -> pure Nothing
    Just (delta1, ex_tvs2, arg_vas) -> do
      -- If we have identical constructors but different existential
      -- tyvars, then generate extra equality constraints to ensure the
      -- existential tyvars. Since ex_tvs2 are fresh, this will never refute.
      -- See Note [Coverage checking and existential tyvars].
      evvars <- listToBag <$> equateTyVars ex_tvs1 ex_tvs2
      let delta2 = delta1 { delta_ty_cs = evvars `unionBags` delta_ty_cs delta1 }
      tracePm "matched at all" (ppr (delta_tm_cs delta2))
      pure (Just (delta2, arg_vas))

{-
Note [Coverage checking and existential tyvars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC's implementation of the pattern-match coverage algorithm (as described in
the GADTs Meet Their Match paper) must take some care to emit enough type
constraints when handling data constructors with exisentially quantified type
variables. To better explain what the challenge is, consider a constructor K
of the form:

  K @e_1 ... @e_m ev_1 ... ev_v ty_1 ... ty_n :: T u_1 ... u_p

Where:

* e_1, ..., e_m are the existentially bound type variables.
* ev_1, ..., ev_v are evidence variables, which may inhabit a dictionary type
  (e.g., Eq) or an equality constraint (e.g., e_1 ~ Int).
* ty_1, ..., ty_n are the types of K's fields.
* T u_1 ... u_p is the return type, where T is the data type constructor, and
  u_1, ..., u_p are the universally quantified type variables.

In the ConVar case, the coverage algorithm will have in hand the constructor
K as well as a pattern variable (pv :: T PV_1 ... PV_p), where PV_1, ..., PV_p
are some types that instantiate u_1, ... u_p. The idea is that we should
substitute PV_1 for u_1, ..., and PV_p for u_p when forming a PmCon (the
mkOneConFull function accomplishes this) and then hand this PmCon off to the
ConCon case.

The presence of existentially quantified type variables adds a significant
wrinkle. We always grab e_1, ..., e_m from the definition of K to begin with,
but we don't want them to appear in the final PmCon, because then
calling (mkOneConFull K) for other pattern variables might reuse the same
existential tyvars, which is certainly wrong.

Previously, GHC's solution to this wrinkle was to always create fresh names
for the existential tyvars and put them into the PmCon. This works well for
many cases, but it can break down if you nest GADT pattern matches in just
the right way. For instance, consider the following program:

    data App f a where
      App :: f a -> App f (Maybe a)

    data Ty a where
      TBool :: Ty Bool
      TInt  :: Ty Int

    data T f a where
      C :: T Ty (Maybe Bool)

    foo :: T f a -> App f a -> ()
    foo C (App TBool) = ()

foo is a total program, but with the previous approach to handling existential
tyvars, GHC would mark foo's patterns as non-exhaustive.

When foo is desugared to Core, it looks roughly like so:

    foo @f @a (C co1 _co2) (App @a1 _co3 (TBool |> co1)) = ()

(Where `a1` is an existential tyvar.)

That, in turn, is processed by the coverage checker to become:

    foo @f @a (C co1 _co2) (App @a1 _co3 (pmvar123 :: f a1))
      | TBool <- pmvar123 |> co1
      = ()

Note that the type of pmvar123 is `f a1`—this will be important later.

Now, we proceed with coverage-checking as usual. When we come to the
ConVar case for App, we create a fresh variable `a2` to represent its
existential tyvar. At this point, we have the equality constraints
`(a ~ Maybe a2, a ~ Maybe Bool, f ~ Ty)` in scope.

However, when we check the guard, it will use the type of pmvar123, which is
`f a1`. Thus, when considering if pmvar123 can match the constructor TInt,
it will generate the constraint `a1 ~ Int`. This means our final set of
equality constraints would be:

    f  ~ Ty
    a  ~ Maybe Bool
    a  ~ Maybe a2
    a1 ~ Int

Which is satisfiable! Freshening the existential tyvar `a` to `a2` doomed us,
because GHC is unable to relate `a2` to `a1`, which really should be the same
tyvar.

Luckily, we can avoid this pitfall. Recall that the ConVar case was where we
generated a PmCon with too-fresh existentials. But after ConVar, we have the
ConCon case, which considers whether each constructor of a particular data type
can be matched on in a particular spot.

In the case of App, when we get to the ConCon case, we will compare our
original App PmCon (from the source program) to the App PmCon created from the
ConVar case. In the former PmCon, we have `a1` in hand, which is exactly the
existential tyvar we want! Thus, we can force `a1` to be the same as `a2` here
by emitting an additional `a1 ~ a2` constraint. Now our final set of equality
constraints will be:

    f  ~ Ty
    a  ~ Maybe Bool
    a  ~ Maybe a2
    a1 ~ Int
    a1 ~ a2

Which is unsatisfiable, as we desired, since we now have that
Int ~ a1 ~ a2 ~ Bool.

In general, App might have more than one constructor, in which case we
couldn't reuse the existential tyvar for App for a different constructor. This
means that we can only use this trick in ConCon when the constructors are the
same. But this is fine, since this is the only scenario where this situation
arises in the first place!
-}


equateTyVars :: [TyVar] -> [TyVar] -> PmM [EvVar]
equateTyVars ex_tvs1 ex_tvs2
  = ASSERT(ex_tvs1 `equalLength` ex_tvs2)
    catMaybes <$> zipWithM mb_to_evvar ex_tvs1 ex_tvs2
  where
    mb_to_evvar tv1 tv2
      | tv1 == tv2 = pure Nothing
      | otherwise  = Just <$> to_evvar tv1 tv2
    to_evvar tv1 tv2 = nameType "pmConCon" $
                       mkPrimEqPred (mkTyVarTy tv1) (mkTyVarTy tv2)

solveVar :: Delta -> Id -> PmExpr -> Maybe Delta
solveVar delta x e =
  case solveOneEq (delta_tm_cs delta) (TVC x e) of
    Nothing -> Nothing
    Just ts -> Just delta{ delta_tm_cs = ts }

-- | Record that a particular 'Id' can't take the shape of a 'PmAltCon' in the
-- 'TmState' and return @Nothing@ if that leads to a contradiction.
-- See Note [Refutable shapes].
tryAddRefutableAltCon :: Delta -> Id -> PmAltCon -> DsM (Maybe Delta)
tryAddRefutableAltCon delta@MkDelta{ delta_tm_cs = ts@(TS env) } x nalt = do
  mb_pos' <- initIncompleteMatches (idType x) pos >>= go
  let new_delta pos = delta{ delta_tm_cs = TS (extendDVarEnv env y pos) }
  pure (new_delta <$> mb_pos')
  where
    (y, VI ty pos neg) = lookupVarInfo ts x
    neg' = combineRefutEntries neg [nalt]

    go NoInfoYet = pure (Just (VI ty NoInfoYet neg'))
    go pos@(CompleteSets im) =
      case nalt of
        PmAltConLike cl -> do
          let im1 = markMatched cl im
          -- The old delta is sufficient here: We have already gotten rid of cl
          let test_inhabited con = isJust <$> mkOneSatisfiableConFull delta x con
          ensureInhabited test_inhabited im1 >>= \case
          ensureInhabited (testInhabited delta con ty) >>= \case
            Nothing -> pure Nothing
            Just im2 -> pure (Just (VI ty (CompleteSets im2) neg'))
        _ -> pure (Just (VI ty pos neg'))
    go pos@(Rigid (exprToAlt -> Just alt)) =
      -- We have to take care to preserve Note [The Pos/Neg invariant]
      pure $ case decEqPmAltCon alt nalt of -- We have a solution
        Just True  -> Nothing               -- ... that is contradictory
        Just False -> Just (VI ty pos neg)  -- ... that is compatible, rendering
                                            --     the refutation redundant
        Nothing    -> Just (VI ty pos neg') -- ... which is incomparable, so
                                            --     might refute later
    go pos = pure (Just (VI ty pos neg'))

testInhabited :: Delta -> ConLike -> Type -> DsM Bool
-- (testInhabited delta K ty) Returns False if
-- a non-bottom value (v::ty) cannot possibly be of form (K _ _ _)
-- Returning True is always sound
--
-- It's like DataCon.dataConCannotMatch, but more clever because it
-- takes the facts in Delta into account

-- | Combines two entries in a 'PmRefutEnv' by merging the set of refutable
-- 'PmAltCon's.
combineRefutEntries :: [PmAltCon] -> [PmAltCon] -> [PmAltCon]
combineRefutEntries old_ncons new_ncons = unionLists old_ncons new_ncons

-- | Is the given variable /rigid/ (i.e., we have a solution for it) or
-- /flexible/ (i.e., no solution)? Returns the solution if /rigid/. A
-- semantically helpful alias for 'lookupVarEnv'.
isRigid :: TmState -> Id -> Maybe PmExpr
isRigid (TS env) x = do
  VI _ pos _ <- lookupDVarEnv env x
  case pos of
    Rigid e -> Just e
    _       -> Nothing

-- | @isFlexible tms = isNothing . 'isRigid' tms@
isFlexible :: TmState -> Id -> Bool
isFlexible ts = isNothing . isRigid ts

-- | Try to unify two 'PmExpr's and record the gained knowledge in the
-- 'TmState'.
--
-- Returns @Nothing@ when there's a contradiction. Returns @Just ts@
-- when the constraint was compatible with prior facts, in which case @ts@ has
-- integrated the knowledge from the equality constraint.
unify :: TmState -> (PmExpr, PmExpr) -> Maybe TmState
unify ts eq@(e1, e2) = case eq of
  (PmExprCon c1 ts1, PmExprCon c2 ts2) -> case decEqPmAltCon c1 c2 of
    -- See Note [Undecidable Equality for PmAltCons]
    Just True -> foldlM unify ts (zip ts1 ts2)
    Just False -> unsat
    Nothing -> boring

  (PmExprVar x, PmExprVar y)
    | x == y    -> boring

  -- It's important to handle both rigid cases before the flexible ones,
  -- otherwise we get cyclic substitutions. Cf. 'extendSubstAndSolve' and
  -- @testsuite/tests/pmcheck/should_compile/CyclicSubst.hs@.
  (PmExprVar x, _)
    | isRefutable x e2 ts -> unsat
  (_, PmExprVar y)
    | isRefutable y e1 ts -> unsat
  (PmExprVar x, _)
    | Just e1' <- isRigid ts x     -> unify ts (e1', e2)
  (_, PmExprVar y)
    | Just e2' <- isRigid ts y     -> unify ts (e1, e2')
  (PmExprVar x, PmExprVar y)       -> Just (equate x y ts)
  (PmExprVar x, PmExprCon c args)  -> trySolve x c args ts
  (PmExprCon c args, PmExprVar y)  -> trySolve y c args ts
  where
    boring    = Just ts
    unsat     = Nothing

-- | Merges the equivalence classes of @x@ and @y@ by extending the substitution
-- with @x :-> y@.
-- Preconditions: @x /= y@ and both @x@ and @y@ are flexible (cf.
-- 'isFlexible'/'isRigid').
equate :: Id -> Id -> TmState -> TmState
equate x y ts@(TS env)
  = ASSERT( x /= y )
    ASSERT( isFlexible ts x )
    ASSERT( isFlexible ts y )
    ts'
  where
    VI ty_x _     neg_x = snd (lookupVarInfo ts x)
    VI ty_y pos_y neg_y = snd (lookupVarInfo ts y)
    vi_x'               = VI ty_x (Rigid (PmExprVar y)) []
    -- Be careful to uphold Note [The Pos/Neg invariant] by merging the refuts
    -- of x into those of y
    -- We could compute the intersection of CompleteSets for pos_x and pos_y
    -- here. Should we? We merge the negs, after all. It's also not so clear
    -- how to merge different COMPLETE sets.
    vi_y' = VI ty_y pos_y (combineRefutEntries neg_x neg_y)
    ts'   = TS (extendDVarEnv (extendDVarEnv env x vi_x') y vi_y')

-- | @trySolve x alt args ts@ extends the substitution with a mapping @x: ->
-- PmExprCon alt args@ if compatible with refutable shapes of @x@ and its
-- solution, reject (@Nothing@) otherwise.
--
-- Precondition: @x@ is flexible (cf. 'isFlexible'/'isRigid').
trySolve:: Id -> PmAltCon -> [PmExpr] -> TmState -> Maybe TmState
trySolve x alt args ts@(TS env)
  | ASSERT( isFlexible ts x )
    isRefutable x e ts
  = Nothing
  | otherwise
  = Just ts'
  where
    e = PmExprCon alt args
    VI ty _ neg = snd (lookupVarInfo ts x)
    -- Uphold Note [The Pos/Neg invariant]
    vi' = VI ty (Rigid e) (filter ((== Nothing) . decEqPmAltCon alt) neg)
    ts' = TS (extendDVarEnv env x vi')

-- | When we know that a variable is fresh, we do not actually have to
-- check whether anything changes, we know that nothing does. Hence,
-- @extendSubst@ simply extends the substitution, unlike what
-- 'extendSubstAndSolve' does.
extendSubst :: Id -> PmExpr -> TmState -> TmState
extendSubst y e (TS env) = TS (extendDVarEnv env y (VI (Rigid e) []))

-- | Apply an (un-flattened) substitution to an expression.
exprDeepLookup :: Delta -> PmExpr -> PmExpr
exprDeepLookup delta (PmExprCon c es) = PmExprCon c (map (exprDeepLookup delta) es)
exprDeepLookup delta (PmExprVar x)
  | Rigid e <- vi_pos (snd (lookupVarInfo (delta_tm_cs delta) x))
  = exprDeepLookup delta e
exprDeepLookup _   e               = e

wrapUpRefutableShapes :: Delta -> DVarEnv [PmAltCon]
wrapUpRefutableShapes MkDelta{ delta_tm_cs = ts@(TS env) }
  = filterDVarEnv notNull (mapDVarEnv f env)
  where
    -- Unfortunate overlap with lookupVarInfo here, because we don't have the
    -- Id
    f (VI _ (Rigid (PmExprVar y)) _) = vi_neg (snd (lookupVarInfo ts y))
    f (VI _ _ neg)                   = neg

-- | External interface to the term oracle.
pmOracle :: Foldable f => TmState -> f TmVarCt -> Maybe TmState
pmOracle tm_state eqs = foldlM solveOneEq tm_state eqs

{- Note [Strict argument type constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

`nonVoid ty` returns True when either:
1. `ty` has at least one InhabitationCandidate for which both its term and type
   constraints are satifiable, and `nonVoid` returns `True` for all of the
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

* Performance considerations

We must be careful when recursively calling `nonVoid` on the strict argument
types of an InhabitationCandidate, because doing so naïvely can cause GHC to
fall into an infinite loop. Consider the following example:

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

Another microoptimization applies to data types like this one:

  data S a = ![a] !T

Even though there is a strict field of type [a], it's quite silly to call
nonVoid on it, since it's "obvious" that it is inhabitable. To make this
intuition formal, we say that a type is definitely inhabitable (DI) if:

  * It has at least one constructor C such that:
    1. C has no equality constraints (since they might be unsatisfiable)
    2. C has no strict argument types (since they might be uninhabitable)

It's relatively cheap to check if a type is DI, so before we call `nonVoid`
on a list of strict argument types, we filter out all of the DI ones.

Note [Refutable shapes]
~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider a pattern match like

    foo x
      | 0 <- x = 42
      | 0 <- x = 43
      | 1 <- x = 44
      | otherwise = 45

This will result in the following initial matching problem:

    PatVec: x     (0 <- x)
    ValVec: $tm_y

Where the first line is the pattern vector and the second line is the value
vector abstraction. When we handle the first pattern guard in Check, it will be
desugared to a match of the form

    PatVec: x     0
    ValVec: $tm_y x

In LitVar, this will split the value vector abstraction for `x` into a positive
`PmLit 0` and a negative `PmLit x [0]` value abstraction. While the former is
immediately matched against the pattern vector, the latter (vector value
abstraction `~[0] $tm_y`) is completely uncovered by the clause.

`pmcheck` proceeds by *discarding* the the value vector abstraction involving
the guard to accomodate for the desugaring. But this also discards the valuable
information that `x` certainly is not the literal 0! Consequently, we wouldn't
be able to report the second clause as redundant.

That's a typical example of why we need the term oracle, and in this specific
case, the ability to encode that `x` certainly is not the literal 0. Now the
term oracle can immediately refute the constraint `x ~ 0` generated by the
second clause and report the clause as redundant. After the third clause, the
set of such *refutable* literals is again extended to `[0, 1]`.

In general, we want to store a set of refutable shapes (`PmAltCon`) for each
variable. That's the purpose of the `PmRefutEnv`. This extends to
`ConLike`s, where all value arguments are universally quantified implicitly.
So, if the `PmRefutEnv` contains an entry for `x` with `Just [Bool]`, then this
corresponds to the fact that `forall y. x ≁ Just @Bool y`.

`tryAddRefutableAltCon` will add such a refutable mapping to the `PmRefutEnv`
in the term oracles state and check if it causes any immediate contradiction.
Whenever we record a solution in the substitution via `extendSubstAndSolve`, the
refutable environment is checked for any matching refutable `PmAltCon`.

Note that `PmAltConLike` carries a list of type arguments. This purely for the
purpose of being able to reconstruct all other constructors of the matching
group the `ConLike` is part of through calling `allCompleteMatches` in Check.
-}
