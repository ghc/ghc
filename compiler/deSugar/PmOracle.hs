{-
Authors: George Karachalias <george.karachalias@cs.kuleuven.be>
         Sebastian Graf <sgraf1337@gmail.com>
         Ryan Scott <ryan.gl.scott@gmail.com>
-}

{-# LANGUAGE CPP, LambdaCase, TupleSections, PatternSynonyms, ViewPatterns, MultiWayIf #-}

-- | The pattern match oracle. The main export of the module are the functions
-- 'addTmCt', 'refineToAltCon' and 'addRefutableAltCon' for adding
-- facts to the oracle, and 'provideEvidenceForEquation' to turn a 'Delta' into
-- a concrete evidence for an equation.
module PmOracle (

        DsM, tracePm, mkPmId,
        Delta, initDelta, canDiverge, lookupRefuts, lookupSolution,
        lookupNumberOfRefinements,

        TmCt(..),
        inhabitants,
        addTypeEvidence,    -- Add type equalities
        addRefutableAltCon, -- Add a negative term equality
        addTmCt,            -- Add a positive term equality x ~ e
        addVarCoreCt,       -- Add a positive term equality x ~ core_expr
        refineToAltCon,     -- Add a positive refinement x ~ K _ _
        tmOracle,           -- Add multiple positive term equalities
        provideEvidenceForEquation,
    ) where

#include "HsVersions.h"

import GhcPrelude

import PmTypes

import DynFlags
import Outputable
import ErrUtils
import Util
import Bag
import UniqDSet
import Unique
import Id
import VarEnv
import UniqDFM
import Var           (EvVar)
import Name
import CoreSyn
import CoreFVs ( exprFreeVars )
import CoreOpt (exprIsConApp_maybe)
import CoreUtils (exprType)
import MkCore (mkListExpr, mkCharExpr)
import UniqSupply
import FastString
import SrcLoc
import ListSetOps (unionLists)
import Maybes
import ConLike
import DataCon
import PatSyn
import TyCon
import TysWiredIn
import TysPrim (tYPETyCon)
import TyCoRep
import Type
import TcSimplify    (tcNormalise, tcCheckSatisfiability)
import TcType        (evVarPred)
import Unify         (tcMatchTy)
import TcRnTypes     (completeMatchConLikes)
import Coercion
import MonadUtils hiding (foldlM)
import DsMonad hiding (foldlM)
import FamInst
import FamInstEnv

import Control.Monad (guard, mzero)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Data.Bifunctor (second)
import Data.Foldable (foldlM)
import Data.List     (find)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Semigroup as Semigroup

-- Debugging Infrastructre

tracePm :: String -> SDoc -> DsM ()
tracePm herald doc = do
  dflags <- getDynFlags
  printer <- mkPrintUnqualifiedDs
  liftIO $ dumpIfSet_dyn_printer printer dflags
            Opt_D_dump_ec_trace (text herald $$ (nest 2 doc))

-- | Generate a fresh `Id` of a given type
mkPmId :: Type -> DsM Id
mkPmId ty = getUniqueM >>= \unique ->
  let occname = mkVarOccFS $ fsLit "$pm"
      name    = mkInternalName unique occname noSrcSpan
  in  return (mkLocalId name ty)

-----------------------------------------------
-- * Caching possible matches of a COMPLETE set

initIM :: Type -> DsM (Maybe PossibleMatches)
initIM ty = case splitTyConApp_maybe ty of
  Nothing -> pure Nothing
  Just (tc, tc_args) -> do
    -- Look into the representation type of a data family instance, too.
    env <- dsGetFamInstEnvs
    let (tc', _tc_args', _co) = tcLookupDataFamInst env tc tc_args
    let mb_rdcs = map RealDataCon <$> tyConDataCons_maybe tc'
    let rdcs = maybeToList mb_rdcs
    -- NB: tc, because COMPLETE sets are associated with the parent data family
    -- TyCon
    pragmas <- dsGetCompleteMatches tc
    let fams = mapM dsLookupConLike . completeMatchConLikes
    pscs <- mapM fams pragmas
    pure (PM tc . fmap mkUniqDSet <$> NonEmpty.nonEmpty (rdcs ++ pscs))

markMatched :: ConLike -> PossibleMatches -> PossibleMatches
markMatched con (PM tc ms) = PM tc (fmap (`delOneFromUniqDSet` con) ms)
markMatched _   NoPM = NoPM

-- | Satisfiability decisions as a data type. The @proof@ can carry a witness
-- for satisfiability and might even be instantiated to 'Data.Void.Void' to
-- degenerate into a semi-decision predicate.
data Satisfiability proof
  = Unsatisfiable
  | PossiblySatisfiable
  | Satisfiable !proof

maybeSatisfiable :: Maybe a -> Satisfiability a
maybeSatisfiable (Just a) = Satisfiable a
maybeSatisfiable Nothing  = Unsatisfiable

-- | Tries to return one of the possible 'ConLike's from one of the COMPLETE
-- sets. If the 'PossibleMatches' was inhabited before (cf. 'ensureInhabited')
-- this 'ConLike' is evidence for that assurance.
getUnmatchedConstructor :: PossibleMatches -> Satisfiability ConLike
getUnmatchedConstructor NoPM = PossiblySatisfiable
getUnmatchedConstructor (PM _tc ms)
  = maybeSatisfiable $ NonEmpty.head <$> traverse pick_one_conlike ms
  where
    pick_one_conlike cs = case uniqDSetToList cs of
      [] -> Nothing
      (cl:_) -> Just cl

---------------------------------------------------
-- * Instantiating constructors, types and evidence

-- | Instantiate a 'ConLike' given its universal type arguments. Instantiates
-- existential and term binders with fresh variables of appropriate type.
-- Also returns instantiated evidence variables from the match and the types of
-- strict constructor fields.
mkOneConFull :: [Type] -> ConLike -> DsM ([Id], Bag TyCt, [Type], [TyVar])
--  * 'con' K is a ConLike
--       - In the case of DataCons and most PatSynCons, these
--         are associated with a particular TyCon T
--       - But there are PatSynCons for this is not the case! See #11336, #17112
--
--  * 'arg_tys' tys are the types K's universally quantified type
--     variables should be instantiated to.
--       - For DataCons and most PatSyns these are the arguments of their TyCon
--       - For cases like in #11336, #17112, the univ_ts include those variables
--         from the view pattern, so tys will have to come from the type checker.
--         They can't easily be recovered from the result type.
--
-- After instantiating the universal tyvars of K to tys we get
--          K @tys :: forall bs. Q => s1 .. sn -> T tys
-- Note that if K is a PatSynCon, depending on arg_tys, T might not necessarily
-- be a concrete TyCon.
--
-- Suppose y1 is a strict field. Then we get
-- Results: [y1,..,yn]
--          Q
--          [s1]
--          [e1,..,en]
mkOneConFull arg_tys con = do
  let (univ_tvs, ex_tvs, eq_spec, thetas, _req_theta , field_tys, _con_res_ty)
        = conLikeFullSig con
  -- pprTrace "mkOneConFull" (ppr con $$ ppr arg_tys $$ ppr univ_tvs $$ ppr _con_res_ty) (return ())
  -- Substitute universals for type arguments
  let subst_univ = zipTvSubst univ_tvs arg_tys
  -- Instantiate fresh existentials as arguments to the contructor
  (subst, ex_tvs') <- cloneTyVarBndrs subst_univ ex_tvs <$> getUniqueSupplyM
  let field_tys' = substTys subst field_tys
  -- Instantiate fresh term variables (VAs) as arguments to the constructor
  vars <- mapM mkPmId field_tys'
  -- All constraints bound by the constructor (alpha-renamed), these are added
  -- to the type oracle
  let ty_cs = map TyCt (substTheta subst (eqSpecPreds eq_spec ++ thetas))
  -- Figure out the types of strict constructor fields
  let arg_is_banged = map isBanged $ conLikeImplBangs con
      strict_arg_tys = filterByList arg_is_banged field_tys'
  return (vars, listToBag ty_cs, strict_arg_tys, ex_tvs')

equateTyVars :: [TyVar] -> [TyVar] -> Bag TyCt
equateTyVars ex_tvs1 ex_tvs2
  = ASSERT(ex_tvs1 `equalLength` ex_tvs2)
    listToBag $ catMaybes $ zipWith mb_to_evvar ex_tvs1 ex_tvs2
  where
    mb_to_evvar tv1 tv2
      | tv1 == tv2 = Nothing
      | otherwise  = Just (to_evvar tv1 tv2)
    to_evvar tv1 tv2 = TyCt $ mkPrimEqPred (mkTyVarTy tv1) (mkTyVarTy tv2)

-------------------------
-- * Pattern match oracle


{- Note [Recovering from unsatisfiable pattern-matching constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following code (see #12957 and #15450):

  f :: Int ~ Bool => ()
  f = case True of { False -> () }

We want to warn that the pattern-matching in `f` is non-exhaustive. But GHC
used not to do this; in fact, it would warn that the match was /redundant/!
This is because the constraint (Int ~ Bool) in `f` is unsatisfiable, and the
coverage checker deems any matches with unsatifiable constraint sets to be
unreachable.

We decide to better than this. When beginning coverage checking, we first
check if the constraints in scope are unsatisfiable, and if so, we start
afresh with an empty set of constraints. This way, we'll get the warnings
that we expect.
-}

-------------------------------------
-- * Composable satisfiability checks

-- | Given a 'Delta', check if it is compatible with new facts encoded in this
-- this check. If so, return 'Just' a potentially extended 'Delta'. Return
-- 'Nothing' if unsatisfiable.
--
-- There are three essential SatisfiabilityChecks:
--   1. 'tmIsSatisfiable', adding term oracle facts
--   2. 'tyIsSatisfiable', adding type oracle facts
--   3. 'tysAreNonVoid', checks if the given types have an inhabitant
-- Functions like 'pmIsSatisfiable', 'nonVoid' and 'testInhabited' plug these
-- together as they see fit.
newtype SatisfiabilityCheck = SC (Delta -> DsM (Maybe Delta))

-- | Check the given 'Delta' for satisfiability by the the given
-- 'SatisfiabilityCheck'. Return 'Just' a new, potentially extended, 'Delta' if
-- successful, and 'Nothing' otherwise.
runSatisfiabilityCheck :: Delta -> SatisfiabilityCheck -> DsM (Maybe Delta)
runSatisfiabilityCheck delta (SC chk) = chk delta

-- | Allowing easy composition of 'SatisfiabilityCheck's.
instance Semigroup SatisfiabilityCheck where
  -- This is @a >=> b@ from MaybeT DsM
  SC a <> SC b = SC c
    where
      c delta = a delta >>= \case
        Nothing     -> pure Nothing
        Just delta' -> b delta'

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
  :: Delta       -- ^ The ambient term and type constraints
                 --   (known to be satisfiable).
  -> Bag TmCt    -- ^ The new term constraints.
  -> Bag TyCt    -- ^ The new type constraints.
  -> [Type]      -- ^ The strict argument types.
  -> DsM (Maybe Delta)
                 -- ^ @'Just' delta@ if the constraints (@delta@) are
                 -- satisfiable, and each strict argument type is inhabitable.
                 -- 'Nothing' otherwise.
pmIsSatisfiable amb_cs new_tm_cs new_ty_cs strict_arg_tys =
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
  | HadRedexes Type [(Type, DataCon)] Type
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
  -- We also keep the type of the DataCon application, so that we don't have to
  -- reconstruct it in inhabitationCandidates.build_newtype.

-- | Just give me the potentially normalised source type, unchanged or not!
normalisedSourceType :: TopNormaliseTypeResult -> Type
normalisedSourceType (NoChange ty)                = ty
normalisedSourceType (NormalisedByConstraints ty) = ty
normalisedSourceType (HadRedexes ty _ _)          = ty

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
-- warnings. See Note [Type normalisation for EmptyCase] for details.
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
       -- "Wrinkle: local equalities" in Note [Type normalisation for EmptyCase].
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
    newTypeStepper :: NormaliseStepper ([Type] -> [Type],[(Type, DataCon)] -> [(Type, DataCon)])
    newTypeStepper rec_nts tc tys
      | Just (ty', _co) <- instNewTyCon_maybe tc tys
      , let orig_ty = TyConApp tc tys
      = case checkRecTc rec_nts tc of
          Just rec_nts' -> let tyf = (orig_ty:)
                               tmf = ((orig_ty, tyConSingleDataCon tc):)
                           in  NS_Step rec_nts' ty' (tyf, tmf)
          Nothing       -> NS_Abort
      | otherwise
      = NS_Done

    tyFamStepper :: FamInstEnvs -> NormaliseStepper ([Type] -> [Type], a -> a)
    tyFamStepper env rec_nts tc tys  -- Try to step a type/data family
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

{- Note [Type normalisation for EmptyCase]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
EmptyCase is an exception for pattern matching, since it is strict. This means
that it boils down to checking whether the type of the scrutinee is inhabited.
Function pmTopNormaliseType gets rid of the outermost type function/data
family redex and newtypes, in search of an algebraic type constructor, which is
easier to check for inhabitation.

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
      constructor and the type we unwrap.
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

  Just (G2, [(G2,MkG2),(G1,MkG1)], R:TInt)

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

-- | Wraps a 'PredType', which is a constraint type.
newtype TyCt = TyCt PredType

instance Outputable TyCt where
  ppr (TyCt pred_ty) = ppr pred_ty

-- | Allocates a fresh 'EvVar' name for 'PredTyCt's, or simply returns the
-- wrapped 'EvVar' for 'EvVarTyCt's.
nameTyCt :: TyCt -> DsM EvVar
nameTyCt (TyCt pred_ty) = do
  unique <- getUniqueM
  let occname = mkVarOccFS (fsLit ("pm_"++show unique))
      idname  = mkInternalName unique occname noSrcSpan
  return (mkLocalId idname pred_ty)

-- | Add some extra type constraints to the 'TyState'; return 'Nothing' if we
-- find a contradiction (e.g. @Int ~ Bool@).
tyOracle :: TyState -> Bag TyCt -> DsM (Maybe TyState)
tyOracle (TySt inert) cts
  = do { evs <- traverse nameTyCt cts
       ; let new_inert = inert `unionBags` evs
       ; ((_warns, errs), res) <- initTcDsForSolver $ tcCheckSatisfiability new_inert
       ; case res of
            -- Note how this implicitly gives all former PredTyCts a name, so
            -- that we don't needlessly re-allocate them every time!
            Just True  -> return (Just (TySt new_inert))
            Just False -> return Nothing
            Nothing    -> pprPanic "tyOracle" (vcat $ pprErrMsgBagWithLoc errs) }

-- | A 'SatisfiabilityCheck' based on new type-level constraints.
-- Returns a new 'Delta' if the new constraints are compatible with existing
-- ones. Doesn't bother calling out to the type oracle if the bag of new type
-- constraints was empty. Will only recheck 'PossibleMatches' in the term oracle
-- for emptiness if the first argument is 'True'.
tyIsSatisfiable :: Bool -> Bag TyCt -> SatisfiabilityCheck
tyIsSatisfiable recheck_complete_sets new_ty_cs = SC $ \delta -> do
  tracePm "tyIsSatisfiable" (ppr new_ty_cs)
  if isEmptyBag new_ty_cs
    then pure (Just delta)
    else tyOracle (delta_ty_st delta) new_ty_cs >>= \case
      Nothing                   -> pure Nothing
      Just ty_st'               -> do
        let delta' = delta{ delta_ty_st = ty_st' }
        if recheck_complete_sets
          then ensureAllPossibleMatchesInhabited delta'
          else pure (Just delta')


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
or are redudant. Examples:
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

The Pos/Neg invariant extends to vi_cache, which stores essentially positive
information. We make sure that vi_neg and vi_cache never overlap. This isn't
strictly necessary since vi_cache is just a cache, so doesn't need to be
accurate: Every suggestion of a possible ConLike from vi_cache might be
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
* Whenever vi_neg subsumes a COMPLETE set, we refute. We consult vi_cache to
  detect this, but we could just compare whole COMPLETE sets to vi_neg every
  time, if it weren't for performance.

Maintaining these invariants in 'addVarVarCt' (the core of the term oracle) and
'addRefutableAltCon' is subtle.
* Merging VarInfos. Example: Add the fact @x ~ y@ (see 'equate').
  - (COMPLETE) If we had @x /~ True@ and @y /~ False@, then we get
    @x /~ [True,False]@. This is vacuous by matter of comparing to the vanilla
    COMPLETE set, so should refute.
  - (Pos/Neg) If we had @x /~ True@ and @y ~ True@, we have to refute.
* Adding positive information. Example: Add the fact @x ~ K ys@ (see 'addVarConCt')
  - (Neg) If we had @x /~ K@, refute.
  - (Pos) If we had @x ~ K2@, and that contradicts the new solution according to
    'eqPmAltCon' (ex. K2 is [] and K is (:)), then refute.
  - (Refine) If we had @x /~ K zs@, unify each y with each z in turn.
* Adding negative information. Example: Add the fact @x /~ Nothing@ (see 'addRefutableAltCon')
  - (Refut) If we have @x ~ K ys@, refute.
  - (Redundant) If we have @x ~ K2@ and @eqPmAltCon K K2 == Disjoint@
    (ex. Just and Nothing), the info is redundant and can be
    discarded.
  - (COMPLETE) If K=Nothing and we had @x /~ Just@, then we get
    @x /~ [Just,Nothing]@. This is vacuous by matter of comparing to the vanilla
    COMPLETE set, so should refute.

Note that merging VarInfo in equate can be done by calling out to 'addVarConCt' and
'addRefutableAltCon' for each of the facts individually.

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
-- Returns a new 'Delta' if the new constraints are compatible with existing
-- ones.
tmIsSatisfiable :: Bag TmCt -> SatisfiabilityCheck
tmIsSatisfiable new_tm_cs = SC $ \delta -> tmOracle delta new_tm_cs

-- | External interface to the term oracle.
tmOracle :: Foldable f => Delta -> f TmCt -> DsM (Maybe Delta)
tmOracle delta = runMaybeT . foldlM go delta
  where
    go delta ct = MaybeT (addTmCt delta ct)

-----------------------
-- * Looking up VarInfo

emptyVarInfo :: Id -> VarInfo
emptyVarInfo x = VI (idType x) [] [] NoPM 0

lookupVarInfo :: TmState -> Id -> VarInfo
-- (lookupVarInfo tms x) tells what we know about 'x'
lookupVarInfo (TmSt env) x = fromMaybe (emptyVarInfo x) (lookupSDIE env x)

initPossibleMatches :: TyState -> VarInfo -> DsM VarInfo
initPossibleMatches ty_st vi@VI{ vi_ty = ty, vi_cache = NoPM } = do
  -- New evidence might lead to refined info on ty, in turn leading to discovery
  -- of a COMPLETE set.
  res <- pmTopNormaliseType ty_st ty
  let ty' = normalisedSourceType res
  mb_pm <- initIM ty'
  -- tracePm "initPossibleMatches" (ppr vi $$ ppr ty' $$ ppr res $$ ppr mb_pm)
  case mb_pm of
    Nothing -> pure vi
    Just pm -> pure vi{ vi_ty = ty', vi_cache = pm }
initPossibleMatches _     vi                                   = pure vi

-- | @initLookupVarInfo ts x@ looks up the 'VarInfo' for @x@ in @ts@ and tries
-- to initialise the 'vi_cache' component if it was 'NoPM' through
-- 'initPossibleMatches'.
initLookupVarInfo :: Delta -> Id -> DsM VarInfo
initLookupVarInfo MkDelta{ delta_tm_st = ts, delta_ty_st = ty_st } x
  = initPossibleMatches ty_st (lookupVarInfo ts x)

------------------------------------------------
-- * Exported utility functions querying 'Delta'

-- | Check whether a constraint (x ~ BOT) can succeed,
-- given the resulting state of the term oracle.
canDiverge :: Delta -> Id -> Bool
canDiverge MkDelta{ delta_tm_st = ts } x
  -- If the variable seems not evaluated, there is a possibility for
  -- constraint x ~ BOT to be satisfiable. That's the case when we haven't found
  -- a solution (i.e. some equivalent literal or constructor) for it yet.
  -- Even if we don't have a solution yet, it might be involved in a negative
  -- constraint, in which case we must already have evaluated it earlier.
  | VI _ [] [] _ _ <- lookupVarInfo ts x
  = True
  -- Variable x is already in WHNF or we know some refutable shape, so the
  -- constraint is non-satisfiable
  | otherwise = False

lookupRefuts :: Uniquable k => Delta -> k -> [PmAltCon]
-- Unfortunately we need the extra bit of polymorphism and the unfortunate
-- duplication of lookupVarInfo here.
lookupRefuts MkDelta{ delta_tm_st = ts@(TmSt (SDIE env)) } k =
  case lookupUDFM env k of
    Nothing -> []
    Just (Indirect y) -> vi_neg (lookupVarInfo ts y)
    Just (Entry vi)   -> vi_neg vi

isDataConSolution :: (PmAltCon, [Id]) -> Bool
isDataConSolution (PmAltConLike (RealDataCon _), _) = True
isDataConSolution _                                 = False

-- @lookupSolution delta x@ picks a single solution ('vi_pos') of @x@ from
-- possibly many, preferring 'RealDataCon' solutions whenever possible.
lookupSolution :: Delta -> Id -> Maybe (PmAltCon, [Id])
lookupSolution delta x = case vi_pos (lookupVarInfo (delta_tm_st delta) x) of
  []                                         -> Nothing
  pos
    | Just sol <- find isDataConSolution pos -> Just sol
    | otherwise                              -> Just (head pos)

-- | @lookupNumberOfRefinements delta x@ Looks up how many times we have refined
-- ('refineToAltCon') @x@ for some 'PmAltCon' to arrive at @delta@. This number
-- is always less or equal to @length (lookupSolution delta x)@!
lookupNumberOfRefinements :: Delta -> Id -> Int
lookupNumberOfRefinements delta x
  = vi_n_refines (lookupVarInfo (delta_tm_st delta) x)

-------------------------------
-- * Adding facts to the oracle

-- | A term constraint. Either equates two variables or a variable with a
-- 'PmAltCon' application.
data TmCt
  = TmVarVar !Id !Id
  | TmVarCon !Id !PmAltCon ![Id]

instance Outputable TmCt where
  ppr (TmVarVar x y)        = ppr x <+> char '~' <+> ppr y
  ppr (TmVarCon x con args) = ppr x <+> char '~' <+> hsep (ppr con : map ppr args)

-- | Add type equalities to 'Delta'.
addTypeEvidence :: Delta -> Bag EvVar -> DsM (Maybe Delta)
addTypeEvidence delta dicts
  = runSatisfiabilityCheck delta (tyIsSatisfiable True (TyCt . evVarPred <$> dicts))

-- | Tries to equate two representatives in 'Delta'.
-- See Note [TmState invariants].
addTmCt :: Delta -> TmCt -> DsM (Maybe Delta)
addTmCt delta ct = runMaybeT $ case ct of
  TmVarVar x y        -> addVarVarCt delta (x, y)
  TmVarCon x con args -> addVarConCt delta x con args

-- | Record that a particular 'Id' can't take the shape of a 'PmAltCon' in the
-- 'Delta' and return @Nothing@ if that leads to a contradiction.
-- See Note [TmState invariants].
addRefutableAltCon :: Delta -> Id -> PmAltCon -> DsM (Maybe Delta)
addRefutableAltCon delta@MkDelta{ delta_tm_st = TmSt env } x nalt = runMaybeT $ do
  vi@(VI _ pos neg pm _) <- lift (initLookupVarInfo delta x)
  -- 1. Bail out quickly when nalt contradicts a solution
  let contradicts nalt (cl, _args) = eqPmAltCon cl nalt == Equal
  guard (not (any (contradicts nalt) pos))
  -- 2. Only record the new fact when it's not already implied by one of the
  -- solutions
  let implies nalt (cl, _args) = eqPmAltCon cl nalt == Disjoint
  let neg'
        | any (implies nalt) pos = neg
        -- See Note [Completeness checking with required Thetas]
        | hasRequiredTheta nalt  = neg
        | otherwise              = unionLists neg [nalt]
  let vi_ext = vi{ vi_neg = neg' }
  -- 3. Make sure there's at least one other possible constructor
  vi' <- case nalt of
    PmAltConLike cl
      -> MaybeT (ensureInhabited delta vi_ext{ vi_cache = markMatched cl pm })
    _ -> pure vi_ext
  pure delta{ delta_tm_st = TmSt (setEntrySDIE env x vi') }

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
The simple solution here is to forget in 'addRefutableAltCon' that we matched
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
-- See Note [Pattern synonym result type] in PatSyn.hs.
guessConLikeUnivTyArgsFromResTy :: FamInstEnvs -> Type -> ConLike -> Maybe [Type]
guessConLikeUnivTyArgsFromResTy env res_ty (RealDataCon _) = do
  (tc, tc_args) <- splitTyConApp_maybe res_ty
  -- Consider data families: In case of a DataCon, we need to translate to
  -- the representation TyCon. For PatSyns, they are relative to the data
  -- family TyCon, so we don't need to translate them.
  let (_, tc_args', _) = tcLookupDataFamInst env tc tc_args
  Just tc_args'
guessConLikeUnivTyArgsFromResTy _   res_ty (PatSynCon ps)  = do
  -- We were successful if we managed to instantiate *every* univ_tv of con.
  -- This is difficult and bound to fail in some cases, see
  -- Note [Pattern synonym result type] in PatSyn.hs. So we just try our best
  -- here and be sure to return an instantiation when we can substitute every
  -- universally quantified type variable.
  -- We *could* instantiate all the other univ_tvs just to fresh variables, I
  -- suppose, but that means we get weird field types for which we don't know
  -- anything. So we prefer to keep it simple here.
  let (univ_tvs,_,_,_,_,con_res_ty) = patSynSig ps
  subst <- tcMatchTy con_res_ty res_ty
  traverse (lookupTyVar subst) univ_tvs

ensureInhabited :: Delta -> VarInfo -> DsM (Maybe VarInfo)
   -- Returns (Just vi) guarantees that at least one member
   -- of each ConLike in the COMPLETE set satisfies the oracle
   --
   -- Internally uses and updates the ConLikeSets in vi_cache.
   --
   -- NB: Does /not/ filter each ConLikeSet with the oracle; members may
   --     remain that do not statisfy it.  This lazy approach just
   --     avoids doing unnecessary work.
ensureInhabited delta vi = fmap (set_cache vi) <$> test (vi_cache vi) -- This would be much less tedious with lenses
  where
    set_cache vi cache = vi { vi_cache = cache }

    test NoPM       = pure (Just NoPM)
    test (PM tc ms) = runMaybeT (PM tc <$> traverse one_set ms)

    one_set cs = find_one_inh cs (uniqDSetToList cs)

    find_one_inh :: ConLikeSet -> [ConLike] -> MaybeT DsM ConLikeSet
    -- (find_one_inh cs cls) iterates over cls, deleting from cs
    -- any uninhabited elements of cls.  Stop (returning Just cs)
    -- when you see an inhabited element; return Nothing if all
    -- are uninhabited
    find_one_inh _  [] = mzero
    find_one_inh cs (con:cons) = lift (inh_test con) >>= \case
      True  -> pure cs
      False -> find_one_inh (delOneFromUniqDSet cs con) cons

    inh_test :: ConLike -> DsM Bool
    -- @inh_test K@ Returns False if a non-bottom value @v::ty@ cannot possibly
    -- be of form @K _ _ _@. Returning True is always sound.
    --
    -- It's like 'DataCon.dataConCannotMatch', but more clever because it takes
    -- the facts in Delta into account.
    inh_test con = do
      env <- dsGetFamInstEnvs
      case guessConLikeUnivTyArgsFromResTy env (vi_ty vi) con of
        Nothing -> pure True -- be conservative about this
        Just arg_tys -> do
          (_vars, ty_cs, strict_arg_tys, _ex_tyvars) <- mkOneConFull arg_tys con
          -- No need to run the term oracle compared to pmIsSatisfiable
          fmap isJust <$> runSatisfiabilityCheck delta $ mconcat
            -- Important to pass False to tyIsSatisfiable here, so that we won't
            -- recursively call ensureAllPossibleMatchesInhabited, leading to an
            -- endless recursion.
            [ tyIsSatisfiable False ty_cs
            , tysAreNonVoid initRecTc strict_arg_tys
            ]

-- | Checks if every 'VarInfo' in the term oracle has still an inhabited
-- 'vi_cache', considering the current type information in 'Delta'.
-- This check is necessary after having matched on a GADT con to weed out
-- impossible matches.
ensureAllPossibleMatchesInhabited :: Delta -> DsM (Maybe Delta)
ensureAllPossibleMatchesInhabited delta@MkDelta{ delta_tm_st = TmSt env }
  = runMaybeT (set_tm_cs_env delta <$> traverseSDIE go env)
  where
    set_tm_cs_env delta env = delta{ delta_tm_st = TmSt env }
    go vi = MaybeT (ensureInhabited delta vi)

-- | @refineToAltCon delta x con arg_tys ex_tyvars@ instantiates @con@ at
-- @arg_tys@ with fresh variables (equating existentials to @ex_tyvars@).
-- It adds a new term equality equating @x@ is to the resulting 'PmAltCon' app
-- and new type equalities arising from GADT matches.
-- If successful, returns the new @delta@ and the fresh term variables, or
-- @Nothing@ otherwise.
refineToAltCon :: Delta -> Id -> PmAltCon -> [Type] -> [TyVar] -> DsM (Maybe (Delta, [Id]))
refineToAltCon delta x l@PmAltLit{}           _arg_tys _ex_tvs1 = runMaybeT $ do
  delta' <- addVarConCt delta x l []
  pure (markRefined delta' x, [])
refineToAltCon delta x alt@(PmAltConLike con) arg_tys  ex_tvs1  = do
  -- The plan for ConLikes:
  -- Suppose K :: forall a b y z. (y,b) -> z -> T a b
  --   where the y,z are the existentials
  -- @refineToAltCon delta x K [ex1, ex2]@ extends delta with the
  --   positive information x :-> K y' z' p q, for some fresh y', z', p, q.
  --   This is done by mkOneConFull.
  --   We return the fresh [p,q] args, and bind the existentials [y',z'] to
  --   [ex1, ex2].
  -- Return Nothing if such a match is contradictory with delta.

  (arg_vars, theta_ty_cs, strict_arg_tys, ex_tvs2) <- mkOneConFull arg_tys con

  -- If we have identical constructors but different existential
  -- tyvars, then generate extra equality constraints to ensure the
  -- existential tyvars.
  -- See Note [Coverage checking and existential tyvars].
  let ex_ty_cs = equateTyVars ex_tvs1 ex_tvs2

  let new_ty_cs = theta_ty_cs `unionBags` ex_ty_cs
  let new_tm_cs = unitBag (TmVarCon x alt arg_vars)

  -- Now check satifiability
  mb_delta <- pmIsSatisfiable delta new_tm_cs new_ty_cs strict_arg_tys
  tracePm "refineToAltCon" (vcat [ ppr x
                                 , ppr new_tm_cs
                                 , ppr new_ty_cs
                                 , ppr strict_arg_tys
                                 , ppr delta
                                 , ppr mb_delta ])
  case mb_delta of
    Nothing     -> pure Nothing
    Just delta' -> pure (Just (markRefined delta' x, arg_vars))

-- | This is the only place that actualy increments 'vi_n_refines'.
markRefined :: Delta -> Id -> Delta
markRefined delta@MkDelta{ delta_tm_st = ts@(TmSt env) } x
  = delta{ delta_tm_st = TmSt env' }
  where
    vi = lookupVarInfo ts x
    env' = setEntrySDIE env x vi{ vi_n_refines = vi_n_refines vi + 1 }

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
K as well as a list of type arguments [t_1, ..., t_n] to substitute T's
universally quantified type variables u_1, ..., u_n for. It's crucial to take
these in as arguments, as it is non-trivial to derive them just from the result
type of a pattern synonym and the ambient type of the match (#11336, #17112).
The type checker already did the hard work, so we should just make use of it.

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

--------------------------------------
-- * Term oracle unification procedure

-- | Try to unify two 'Id's and record the gained knowledge in 'Delta'.
--
-- Returns @Nothing@ when there's a contradiction. Returns @Just delta@
-- when the constraint was compatible with prior facts, in which case @delta@
-- has integrated the knowledge from the equality constraint.
--
-- See Note [TmState invariants].
addVarVarCt :: Delta -> (Id, Id) -> MaybeT DsM Delta
addVarVarCt delta@MkDelta{ delta_tm_st = TmSt env } (x, y)
  -- It's important that we never @equate@ two variables of the same equivalence
  -- class, otherwise we might get cyclic substitutions.
  -- Cf. 'extendSubstAndSolve' and
  -- @testsuite/tests/pmcheck/should_compile/CyclicSubst.hs@.
  | sameRepresentativeSDIE env x y = pure delta
  | otherwise                      = equate delta x y

-- | @equate ts@(TmSt env) x y@ merges the equivalence classes of @x@ and @y@ by
-- adding an indirection to the environment.
-- Makes sure that the positive and negative facts of @x@ and @y@ are
-- compatible.
-- Preconditions: @not (sameRepresentativeSDIE env x y)@
--
-- See Note [TmState invariants].
equate :: Delta -> Id -> Id -> MaybeT DsM Delta
equate delta@MkDelta{ delta_tm_st = TmSt env } x y
  = ASSERT( not (sameRepresentativeSDIE env x y) )
    case (lookupSDIE env x, lookupSDIE env y) of
      (Nothing, _) -> pure (delta{ delta_tm_st = TmSt (setIndirectSDIE env x y) })
      (_, Nothing) -> pure (delta{ delta_tm_st = TmSt (setIndirectSDIE env y x) })
      -- Merge the info we have for x into the info for y
      (Just vi_x, Just vi_y) -> do
        -- This assert will probably trigger at some point...
        -- We should decide how to break the tie
        MASSERT2( vi_ty vi_x `eqType` vi_ty vi_y, text "Not same type" )
        -- First assume that x and y are in the same equivalence class
        let env_ind = setIndirectSDIE env x y
        -- Then sum up the refinement counters
        let vi_y' = vi_y{ vi_n_refines = vi_n_refines vi_x + vi_n_refines vi_y }
        let env_refs = setEntrySDIE env_ind y vi_y'
        let delta_refs = delta{ delta_tm_st = TmSt env_refs }
        -- and then gradually merge every positive fact we have on x into y
        let add_fact delta (cl, args) = addVarConCt delta y cl args
        delta_pos <- foldlM add_fact delta_refs (vi_pos vi_x)
        -- Do the same for negative info
        let add_refut delta nalt = MaybeT (addRefutableAltCon delta y nalt)
        delta_neg <- foldlM add_refut delta_pos (vi_neg vi_x)
        -- vi_cache will be updated in addRefutableAltCon, so we are good to
        -- go!
        pure delta_neg

-- | @addVarConCt x alt args ts@ extends the substitution with a solution
-- @x :-> (alt, args)@ if compatible with refutable shapes of @x@ and its
-- other solutions, reject (@Nothing@) otherwise.
--
-- See Note [TmState invariants].
addVarConCt :: Delta -> Id -> PmAltCon -> [Id] -> MaybeT DsM Delta
addVarConCt delta@MkDelta{ delta_tm_st = TmSt env } x alt args = do
  VI ty pos neg cache n <- lift (initLookupVarInfo delta x)
  -- First try to refute with a negative fact
  guard (all ((/= Equal) . eqPmAltCon alt) neg)
  -- Then see if any of the other solutions (remember: each of them is an
  -- additional refinement of the possible values x could take) indicate a
  -- contradiction
  guard (all ((/= Disjoint) . eqPmAltCon alt . fst) pos)
  -- Now we should be good! Add (alt, args) as a possible solution, or refine an
  -- existing one
  case find ((== Equal) . eqPmAltCon alt . fst) pos of
    Just (_, other_args) -> do
      foldlM addVarVarCt delta (zip args other_args)
    Nothing -> do
      -- Filter out redundant negative facts (those that compare Just False to
      -- the new solution)
      let neg' = filter ((== PossiblyOverlap) . eqPmAltCon alt) neg
      let pos' = (alt,args):pos
      pure delta{ delta_tm_st = TmSt (setEntrySDIE env x (VI ty pos' neg' cache n))}

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
  { ic_tm_cs          :: Bag TmCt
  , ic_ty_cs          :: Bag TyCt
  , ic_strict_arg_tys :: [Type]
  }

instance Outputable InhabitationCandidate where
  ppr (InhabitationCandidate tm_cs ty_cs strict_arg_tys) =
    text "InhabitationCandidate" <+>
      vcat [ text "ic_tm_cs          =" <+> ppr tm_cs
           , text "ic_ty_cs          =" <+> ppr ty_cs
           , text "ic_strict_arg_tys =" <+> ppr strict_arg_tys ]

mkInhabitationCandidate :: Id -> DataCon -> DsM InhabitationCandidate
-- Precondition: idType x is a TyConApp, so that tyConAppArgs in here is safe.
mkInhabitationCandidate x dc = do
  let cl = RealDataCon dc
  let tc_args = tyConAppArgs (idType x)
  (arg_vars, ty_cs, strict_arg_tys, _ex_tyvars) <- mkOneConFull tc_args cl
  pure InhabitationCandidate
        { ic_tm_cs = unitBag (TmVarCon x (PmAltConLike cl) arg_vars)
        , ic_ty_cs = ty_cs
        , ic_strict_arg_tys = strict_arg_tys
        }

-- | Generate all 'InhabitationCandidate's for a given type. The result is
-- either @'Left' ty@, if the type cannot be reduced to a closed algebraic type
-- (or if it's one trivially inhabited, like 'Int'), or @'Right' candidates@,
-- if it can. In this case, the candidates are the signature of the tycon, each
-- one accompanied by the term- and type- constraints it gives rise to.
-- See also Note [Checking EmptyCase Expressions]
inhabitationCandidates :: Delta -> Type
                       -> DsM (Either Type (TyCon, Id, [InhabitationCandidate]))
inhabitationCandidates MkDelta{ delta_ty_st = ty_st } ty = do
  pmTopNormaliseType ty_st ty >>= \case
    NoChange _                    -> alts_to_check ty     ty      []
    NormalisedByConstraints ty'   -> alts_to_check ty'    ty'     []
    HadRedexes src_ty dcs core_ty -> alts_to_check src_ty core_ty dcs
  where
    -- All these types are trivially inhabited
    trivially_inhabited = [ charTyCon, doubleTyCon, floatTyCon
                          , intTyCon, wordTyCon, word8TyCon ]

    build_newtype :: (Type, DataCon) -> Id -> DsM (Id, TmCt)
    build_newtype (ty, dc) x = do
      -- ty is the type of @dc x@. It's a @dataConTyCon dc@ application.
      y <- mkPmId ty
      pure (y, TmVarCon y (PmAltConLike (RealDataCon dc)) [x])

    build_newtypes :: Id -> [(Type, DataCon)] -> DsM (Id, [TmCt])
    build_newtypes x = foldrM (\dc (x, cts) -> go dc x cts) (x, [])
      where
        go dc x cts = second (:cts) <$> build_newtype dc x

    -- Inhabitation candidates, using the result of pmTopNormaliseType
    alts_to_check :: Type -> Type -> [(Type, DataCon)]
                  -> DsM (Either Type (TyCon, Id, [InhabitationCandidate]))
    alts_to_check src_ty core_ty dcs = case splitTyConApp_maybe core_ty of
      Just (tc, _)
        |  tc `elem` trivially_inhabited
        -> case dcs of
             []    -> return (Left src_ty)
             (_:_) -> do inner <- mkPmId core_ty
                         (outer, new_tm_cts) <- build_newtypes inner dcs
                         return $ Right (tc, outer, [InhabitationCandidate
                           { ic_tm_cs = listToBag new_tm_cts
                           , ic_ty_cs = emptyBag, ic_strict_arg_tys = [] }])

        |  pmIsClosedType core_ty && not (isAbstractTyCon tc)
           -- Don't consider abstract tycons since we don't know what their
           -- constructors are, which makes the results of coverage checking
           -- them extremely misleading.
        -> do
             inner <- mkPmId core_ty -- it would be wrong to unify inner
             alts <- mapM (mkInhabitationCandidate inner) (tyConDataCons tc)
             (outer, new_tm_cts) <- build_newtypes inner dcs
             let wrap_dcs alt = alt{ ic_tm_cs = listToBag new_tm_cts `unionBags` ic_tm_cs alt}
             return $ Right (tc, outer, map wrap_dcs alts)
      -- For other types conservatively assume that they are inhabited.
      _other -> return (Left src_ty)

inhabitants :: Delta -> Type -> DsM (Either Type (Id, [Delta]))
inhabitants delta ty = inhabitationCandidates delta ty >>= \case
  Left ty' -> pure (Left ty')
  Right (_, va, candidates) -> do
    deltas <- flip mapMaybeM candidates $
        \InhabitationCandidate{ ic_tm_cs = tm_cs
                              , ic_ty_cs = ty_cs
                              , ic_strict_arg_tys = strict_arg_tys } -> do
      pmIsSatisfiable delta tm_cs ty_cs strict_arg_tys
    pure (Right (va, deltas))

----------------------------
-- * Detecting vacuous types

{- Note [Checking EmptyCase Expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Empty case expressions are strict on the scrutinee. That is, `case x of {}`
will force argument `x`. Hence, `checkMatches` is not sufficient for checking
empty cases, because it assumes that the match is not strict (which is true
for all other cases, apart from EmptyCase). This gave rise to #10746. Instead,
we do the following:

1. We normalise the outermost type family redex, data family redex or newtype,
   using pmTopNormaliseType (in types/FamInstEnv.hs). This computes 3
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

-- | A 'SatisfiabilityCheck' based on "NonVoid ty" constraints, e.g. Will
-- check if the @strict_arg_tys@ are actually all inhabited.
-- Returns the old 'Delta' if all the types are non-void according to 'Delta'.
tysAreNonVoid :: RecTcChecker -> [Type] -> SatisfiabilityCheck
tysAreNonVoid rec_env strict_arg_tys = SC $ \delta -> do
  all_non_void <- checkAllNonVoid rec_env delta strict_arg_tys
  -- Check if each strict argument type is inhabitable
  pure $ if all_non_void
            then Just delta
            else Nothing

-- | Implements two performance optimizations, as described in
-- @Note [Strict argument type constraints]@.
checkAllNonVoid :: RecTcChecker -> Delta -> [Type] -> DsM Bool
checkAllNonVoid rec_ts amb_cs strict_arg_tys = do
  let definitely_inhabited = definitelyInhabitedType (delta_ty_st amb_cs)
  tys_to_check <- filterOutM definitely_inhabited strict_arg_tys
  let rec_max_bound | tys_to_check `lengthExceeds` 1
                    = 1
                    | otherwise
                    = defaultRecTcMaxBound
      rec_ts' = setRecTcMaxBound rec_max_bound rec_ts
  allM (nonVoid rec_ts' amb_cs) tys_to_check

-- | Checks if a strict argument type of a conlike is inhabitable by a
-- terminating value (i.e, an 'InhabitationCandidate').
-- See @Note [Strict argument type constraints]@.
nonVoid
  :: RecTcChecker -- ^ The per-'TyCon' recursion depth limit.
  -> Delta        -- ^ The ambient term/type constraints (known to be
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
    cand_is_inhabitable :: RecTcChecker -> Delta
                        -> InhabitationCandidate -> DsM Bool
    cand_is_inhabitable rec_ts amb_cs
      (InhabitationCandidate{ ic_tm_cs          = new_tm_cs
                            , ic_ty_cs          = new_ty_cs
                            , ic_strict_arg_tys = new_strict_arg_tys }) =
        fmap isJust $ runSatisfiabilityCheck amb_cs $ mconcat
          [ tyIsSatisfiable False new_ty_cs
          , tmIsSatisfiable new_tm_cs
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
    meets_criteria :: (Type, DataCon) -> Bool
    meets_criteria (_, con) =
      null (dataConEqSpec con) && -- (1)
      null (dataConImplBangs con) -- (2)

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
-}

--------------------------------------------
-- * Providing positive evidence for a Delta

-- | @provideEvidenceForEquation vs n delta@ returns a list of
-- at most @n@ (but perhaps empty) refinements of @delta@ that instantiate
-- @vs@ to compatible constructor applications or wildcards.
-- Negative information is only retained if literals are involved or when
-- for recursive GADTs.
provideEvidenceForEquation :: [Id] -> Int -> Delta -> DsM [Delta]
provideEvidenceForEquation = go init_ts
  where
    -- Choosing 1 here will not be enough for RedBlack, but any other bound
    -- might potentially lead to combinatorial explosion, so we are extremely
    -- cautious and pick 2 here.
    init_ts                  = setRecTcMaxBound 2 initRecTc
    go _      _      0 _     = pure []
    go _      []     _ delta = pure [delta]
    go rec_ts (x:xs) n delta = do
      VI ty pos neg pm _ <- initLookupVarInfo delta x
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
          let arg_vas = concatMap (\(_cl, args) -> args) pos
          go rec_ts (arg_vas ++ xs) n delta
        []
          -- First the simple case where we don't need to query the oracles
          | isVanillaDataType ty
              -- So no type info unleashed in pattern match
          , null neg
              -- No term-level info either
          || notNull [ l | PmAltLit l <- neg ]
              -- ... or there are literals involved, in which case we don't want
              -- to split on possible constructors
          -> go rec_ts xs n delta
        [] -> do
          -- We have to pick one of the available constructors and try it
          -- It's important that each of the ConLikeSets in pm is still inhabited,
          -- so that it doesn't matter from which we pick.
          -- I think we implicitly uphold that invariant, but not too sure
          case getUnmatchedConstructor pm of
            Unsatisfiable -> pure []
            -- No COMPLETE sets available, so we can assume it's inhabited
            PossiblySatisfiable -> go rec_ts xs n delta
            Satisfiable cl
              | Just rec_ts' <- checkRecTc rec_ts (fst (splitTyConApp ty))
              -> split_at_con rec_ts' delta n x xs cl
              | otherwise
              -- We ran out of fuel; just conservatively assume that this is
              -- inhabited.
              -> go rec_ts xs n delta

    -- | @split_at_con _ delta _ x _ con@ splits the given delta into two
    -- models: One where we assume x is con and one where we assume it can't be
    -- con. Really similar to the ConVar case in Check, only that we don't
    -- really have a pattern driving the split.
    split_at_con
      :: RecTcChecker -- ^ Detects when we split the same TyCon too often
      -> Delta        -- ^ The model we like to refine to the split
      -> Int          -- ^ The number of equations still to produce
      -> Id -> [Id]   -- ^ Head and tail of the value abstractions
      -> ConLike      -- ^ The ConLike over which to split
      -> DsM [Delta]
    split_at_con rec_ts delta n x xs cl = do
      -- This will be really similar to the ConVar case
      let (_,ex_tvs,_,_,_,_,_) = conLikeFullSig cl
          -- we might need to freshen ex_tvs. Not sure
      -- We may need to reduce type famlies/synonyms in x's type first
      res <- pmTopNormaliseType (delta_ty_st delta) (idType x)
      let res_ty = normalisedSourceType res
      env <- dsGetFamInstEnvs
      case guessConLikeUnivTyArgsFromResTy env res_ty cl of
        Nothing      -> pure [delta] -- We can't split this one, so assume it's inhabited
        Just arg_tys -> do
          ev_pos <- refineToAltCon delta x (PmAltConLike cl) arg_tys ex_tvs >>= \case
            Nothing                -> pure []
            Just (delta', arg_vas) ->
              go rec_ts (arg_vas ++ xs) n delta'

          -- Only n' more equations to go...
          let n' = n - length ev_pos
          ev_neg <- addRefutableAltCon delta x (PmAltConLike cl) >>= \case
            Nothing                          -> pure []
            Just delta'                      -> go rec_ts (x:xs) n' delta'

          -- Actually there was no need to split if we see that both branches
          -- were inhabited and we had no negative information on the variable!
          -- So only refine delta if we find that one branch was indeed
          -- uninhabited.
          let neg = lookupRefuts delta x
          case (ev_pos, ev_neg) of
            ([], _)       -> pure ev_neg
            (_, [])       -> pure ev_pos
            _ | null neg  -> pure [delta]
              | otherwise -> pure (ev_pos ++ ev_neg)

-- | Checks if every data con of the type 'isVanillaDataCon'.
isVanillaDataType :: Type -> Bool
isVanillaDataType ty = fromMaybe False $ do
  (tc, _) <- splitTyConApp_maybe ty
  dcs <- tyConDataCons_maybe tc
  pure (all isVanillaDataCon dcs)

-- Most of our actions thread around a delta from one computation to the next,
-- thereby potentially failing. This is expressed in the following Monad:
-- type PmM a = StateT Delta (MaybeT DsM) a

-- | Records that a variable @x@ is equal to a 'CoreExpr' @e@.
addVarCoreCt :: Delta -> Id -> CoreExpr -> DsM (Maybe Delta)
addVarCoreCt delta x e = runMaybeT (execStateT (core_expr x e) delta)
  where
    -- | Takes apart a 'CoreExpr' and tries to extract as much information about
    -- literals and constructor applications as possible.
    core_expr :: Id -> CoreExpr -> StateT Delta (MaybeT DsM) ()
    -- TODO: Handle newtypes properly, by wrapping the expression in a DataCon
    core_expr x (Cast e _co) = core_expr x e
    core_expr x (Tick _t e) = core_expr x e
    core_expr x e
      | Just (pmLitAsStringLit -> Just s) <- coreExprAsPmLit e
      , expr_ty `eqType` stringTy
      -- See Note [Representation of Strings in TmState]
      = case unpackFS s of
          -- We need this special case to break a loop with coreExprAsPmLit
          -- Otherwise we alternate endlessly between [] and ""
          [] -> data_con_app x nilDataCon []
          s' -> core_expr x (mkListExpr charTy (map mkCharExpr s'))
      | Just lit <- coreExprAsPmLit e
      = pm_lit x lit
      | Just (_in_scope, _empty_floats@[], dc, _arg_tys, args)
            <- exprIsConApp_maybe in_scope_env e
      = do { arg_ids <- traverse bind_expr args
           ; data_con_app x dc arg_ids }
      -- TODO: Think about how to recognize PatSyns
      | Var y <- e, not (isDataConWorkId x)
      = modifyT (\delta -> addVarVarCt delta (x, y))
      | otherwise
      -- TODO: Use a CoreMap to identify the CoreExpr with a unique representant
      = pure ()
      where
        expr_ty       = exprType e
        expr_in_scope = mkInScopeSet (exprFreeVars e)
        in_scope_env  = (expr_in_scope, const NoUnfolding)
        -- It's inconvenient to get hold of a global in-scope set
        -- here, but it'll only be needed if exprIsConApp_maybe ends
        -- up substituting inside a forall or lambda (i.e. seldom)
        -- so using exprFreeVars seems fine.   See MR !1647.

        bind_expr :: CoreExpr -> StateT Delta (MaybeT DsM) Id
        bind_expr e = do
          x <- lift (lift (mkPmId (exprType e)))
          core_expr x e
          pure x

    data_con_app :: Id -> DataCon -> [Id] -> StateT Delta (MaybeT DsM) ()
    data_con_app x dc args = pm_alt_con_app x (PmAltConLike (RealDataCon dc)) args

    pm_lit :: Id -> PmLit -> StateT Delta (MaybeT DsM) ()
    pm_lit x lit = pm_alt_con_app x (PmAltLit lit) []

    -- | Adds the given constructor application as a solution for @x@.
    pm_alt_con_app :: Id -> PmAltCon -> [Id] -> StateT Delta (MaybeT DsM) ()
    pm_alt_con_app x con args = modifyT $ \delta -> addVarConCt delta x con args

-- | Like 'modify', but with an effectful modifier action
modifyT :: Monad m => (s -> m s) -> StateT s m ()
modifyT f = StateT $ fmap ((,) ()) . f
