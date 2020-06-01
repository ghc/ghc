{-
Authors: George Karachalias <george.karachalias@cs.kuleuven.be>
         Sebastian Graf <sgraf1337@gmail.com>
         Ryan Scott <ryan.gl.scott@gmail.com>
-}

{-# LANGUAGE CPP, LambdaCase, TupleSections, PatternSynonyms, ViewPatterns, MultiWayIf #-}

-- | The pattern match oracle. The main export of the module are the functions
-- 'addPmCts' for adding facts to the oracle, and 'provideEvidence' to turn a
-- 'Delta' into a concrete evidence for an equation.
module GHC.HsToCore.PmCheck.Oracle (

        DsM, tracePm, mkPmId,
        Delta, initDeltas, lookupRefuts, lookupSolution,

        PmCt(PmTyCt), PmCts, pattern PmVarCt, pattern PmCoreCt,
        pattern PmConCt, pattern PmNotConCt, pattern PmBotCt,
        pattern PmNotBotCt,

        addPmCts,           -- Add a constraint to the oracle.
        canDiverge,         -- Try to add the term equality x ~ ⊥
        provideEvidence
    ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.HsToCore.PmCheck.Types

import GHC.Driver.Session
import GHC.Utils.Outputable
import GHC.Utils.Error
import GHC.Utils.Misc
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
import GHC.Tc.Types      (completeMatchConLikes)
import GHC.Core.Coercion
import GHC.Utils.Monad hiding (foldlM)
import GHC.HsToCore.Monad hiding (foldlM)
import GHC.Tc.Instance.Family
import GHC.Core.FamInstEnv

import Control.Monad (guard, mzero, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Data.Bifunctor (second)
import Data.Either   (partitionEithers)
import Data.Foldable (foldlM, minimumBy, toList)
import Data.List     (find)
import qualified Data.List.NonEmpty as NonEmpty
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
  in  return (mkLocalIdOrCoVar name ty)

-----------------------------------------------
-- * Caching possible matches of a COMPLETE set

markMatched :: ConLike -> PossibleMatches -> PossibleMatches
markMatched _   NoPM    = NoPM
markMatched con (PM ms) = PM (del_one_con con <$> ms)
  where
    del_one_con = flip delOneFromUniqDSet

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
  let field_tys' = substTys subst field_tys
  -- Instantiate fresh term variables (VAs) as arguments to the constructor
  vars <- mapM mkPmId field_tys'
  -- All constraints bound by the constructor (alpha-renamed), these are added
  -- to the type oracle
  let ty_cs = substTheta subst (eqSpecPreds eq_spec ++ thetas)
  -- Figure out the types of strict constructor fields
  let arg_is_strict
        | RealDataCon dc <- con
        , isNewTyCon (dataConTyCon dc)
        = [True] -- See Note [Divergence of Newtype matches]
        | otherwise
        = map isBanged $ conLikeImplBangs con
      strict_arg_tys = filterByList arg_is_strict field_tys'
  return (ex_tvs, vars, listToBag ty_cs, strict_arg_tys)

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
coverage checker deems any matches with unsatisfiable constraint sets to be
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
  -> Bag TyCt    -- ^ The new type constraints.
  -> Bag TmCt    -- ^ The new term constraints.
  -> [Type]      -- ^ The strict argument types.
  -> DsM (Maybe Delta)
                 -- ^ @'Just' delta@ if the constraints (@delta@) are
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
  return (mkLocalIdOrCoVar idname pred_ty)

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
-- Returns a new 'Delta' if the new constraints are compatible with existing
-- ones. Doesn't bother calling out to the type oracle if the bag of new type
-- constraints was empty. Will only recheck 'PossibleMatches' in the term oracle
-- for emptiness if the first argument is 'True'.
tyIsSatisfiable :: Bool -> Bag PredType -> SatisfiabilityCheck
tyIsSatisfiable recheck_complete_sets new_ty_cs = SC $ \delta ->
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
-- Returns a new 'Delta' if the new constraints are compatible with existing
-- ones.
tmIsSatisfiable :: Bag TmCt -> SatisfiabilityCheck
tmIsSatisfiable new_tm_cs = SC $ \delta -> runMaybeT $ foldlM addTmCt delta new_tm_cs

-----------------------
-- * Looking up VarInfo

emptyVarInfo :: Id -> VarInfo
emptyVarInfo x = VI (idType x) [] emptyPmAltConSet NoPM

lookupVarInfo :: TmState -> Id -> VarInfo
-- (lookupVarInfo tms x) tells what we know about 'x'
lookupVarInfo (TmSt env _) x = fromMaybe (emptyVarInfo x) (lookupSDIE env x)

initPossibleMatches :: TyState -> VarInfo -> DsM VarInfo
initPossibleMatches ty_st vi@VI{ vi_ty = ty, vi_cache = NoPM } = do
  -- New evidence might lead to refined info on ty, in turn leading to discovery
  -- of a COMPLETE set.
  res <- pmTopNormaliseType ty_st ty
  let ty' = normalisedSourceType res
  case splitTyConApp_maybe ty' of
    Nothing -> pure vi{ vi_ty = ty' }
    Just (tc, [_])
      | tc == tYPETyCon
      -- TYPE acts like an empty data type on the term-level (#14086), but
      -- it is a PrimTyCon, so tyConDataCons_maybe returns Nothing. Hence a
      -- special case.
      -> pure vi{ vi_ty = ty', vi_cache = PM (pure emptyUniqDSet) }
    Just (tc, tc_args) -> do
      -- See Note [COMPLETE sets on data families]
      (tc_rep, tc_fam) <- case tyConFamInst_maybe tc of
        Just (tc_fam, _) -> pure (tc, tc_fam)
        Nothing -> do
          env <- dsGetFamInstEnvs
          let (tc_rep, _tc_rep_args, _co) = tcLookupDataFamInst env tc tc_args
          pure (tc_rep, tc)
      -- Note that the common case here is tc_rep == tc_fam
      let mb_rdcs = map RealDataCon <$> tyConDataCons_maybe tc_rep
      let rdcs = maybeToList mb_rdcs
      -- NB: tc_fam, because COMPLETE sets are associated with the parent data
      -- family TyCon
      pragmas <- dsGetCompleteMatches tc_fam
      let fams = mapM dsLookupConLike . completeMatchConLikes
      pscs <- mapM fams pragmas
      -- pprTrace "initPossibleMatches" (ppr ty $$ ppr ty' $$ ppr tc_rep <+> ppr tc_fam <+> ppr tc_args $$ ppr (rdcs ++ pscs)) (return ())
      case NonEmpty.nonEmpty (rdcs ++ pscs) of
        Nothing -> pure vi{ vi_ty = ty' } -- Didn't find any COMPLETE sets
        Just cs -> pure vi{ vi_ty = ty', vi_cache = PM (mkUniqDSet <$> cs) }
initPossibleMatches _     vi                                   = pure vi

-- | @initLookupVarInfo ts x@ looks up the 'VarInfo' for @x@ in @ts@ and tries
-- to initialise the 'vi_cache' component if it was 'NoPM' through
-- 'initPossibleMatches'.
initLookupVarInfo :: Delta -> Id -> DsM VarInfo
initLookupVarInfo MkDelta{ delta_tm_st = ts, delta_ty_st = ty_st } x
  = initPossibleMatches ty_st (lookupVarInfo ts x)

{- Note [COMPLETE sets on data families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
User-defined COMPLETE sets involving data families are attached to the family
TyCon, whereas the built-in COMPLETE set is attached to a data family instance's
representation TyCon. This matters for COMPLETE sets involving both DataCons
and PatSyns (from #17207):

  data family T a
  data family instance T () = A | B
  pattern C = B
  {-# COMPLETE A, C #-}
  f :: T () -> ()
  f A = ()
  f C = ()

The match on A is actually wrapped in a CoPat, matching impedance between T ()
and its representation TyCon, which we translate as
@x | let y = x |> co, A <- y@ in PmCheck.

Which TyCon should we use for looking up the COMPLETE set? The representation
TyCon from the match on A would only reveal the built-in COMPLETE set, while the
data family TyCon would only give the user-defined one. But when initialising
the PossibleMatches for a given Type, we want to do so only once, because
merging different COMPLETE sets after the fact is very complicated and possibly
inefficient.

So in fact, we just *drop* the coercion arising from the CoPat when handling
handling the constraint @y ~ x |> co@ in addCoreCt, just equating @y ~ x@.
We then handle the fallout in initPossibleMatches, which has to get a hand at
both the representation TyCon tc_rep and the parent data family TyCon tc_fam.
It considers three cases after having established that the Type is a TyConApp:

1. The TyCon is a vanilla data type constructor
2. The TyCon is tc_rep
3. The TyCon is tc_fam

1. is simple and subsumed by the handling of the other two.
We check for case 2. by 'tyConFamInst_maybe' and get the tc_fam out.
Otherwise (3.), we try to lookup the data family instance at that particular
type to get out the tc_rep. In case 1., this will just return the original
TyCon, so tc_rep = tc_fam afterwards.
-}

------------------------------------------------
-- * Exported utility functions querying 'Delta'

-- | Check whether adding a constraint @x ~ BOT@ to 'Delta' succeeds.
canDiverge :: Delta -> Id -> Bool
canDiverge delta@MkDelta{ delta_tm_st = ts } x
  | VI _ pos neg _ <- lookupVarInfo ts x
  = isEmptyPmAltConSet neg && all pos_can_diverge pos
  where
    pos_can_diverge (PmAltConLike (RealDataCon dc), _, [y])
      -- See Note [Divergence of Newtype matches]
      | isNewTyCon (dataConTyCon dc) = canDiverge delta y
    pos_can_diverge _ = False

{- Note [Divergence of Newtype matches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Newtypes behave rather strangely when compared to ordinary DataCons. In a
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
1. When compiling the PmCon guard in 'pmCompileTree', don't add a @DivergeIf@,
   because the match will never diverge.
2. Regard @T2 x@ as 'canDiverge' iff @x@ 'canDiverge'. E.g. @T2 x ~ _|_@ <=>
   @x ~ _|_@. This way, the third clause will still be marked as inaccessible
   RHS instead of redundant.
3. When testing for inhabitants ('mkOneConFull'), we regard the newtype field as
   strict, so that the newtype is inhabited iff its field is inhabited.
-}

lookupRefuts :: Uniquable k => Delta -> k -> [PmAltCon]
-- Unfortunately we need the extra bit of polymorphism and the unfortunate
-- duplication of lookupVarInfo here.
lookupRefuts MkDelta{ delta_tm_st = ts@(TmSt (SDIE env) _) } k =
  case lookupUDFM env k of
    Nothing -> []
    Just (Indirect y) -> pmAltConSetElems (vi_neg (lookupVarInfo ts y))
    Just (Entry vi)   -> pmAltConSetElems (vi_neg vi)

isDataConSolution :: (PmAltCon, [TyVar], [Id]) -> Bool
isDataConSolution (PmAltConLike (RealDataCon _), _, _) = True
isDataConSolution _                                    = False

-- @lookupSolution delta x@ picks a single solution ('vi_pos') of @x@ from
-- possibly many, preferring 'RealDataCon' solutions whenever possible.
lookupSolution :: Delta -> Id -> Maybe (PmAltCon, [TyVar], [Id])
lookupSolution delta x = case vi_pos (lookupVarInfo (delta_tm_st delta) x) of
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

-- | Adds new constraints to 'Delta' and returns 'Nothing' if that leads to a
-- contradiction.
addPmCts :: Delta -> PmCts -> DsM (Maybe Delta)
-- See Note [TmState invariants].
addPmCts delta cts = do
  let (ty_cts, tm_cts) = partitionTyTmCts cts
  runSatisfiabilityCheck delta $ mconcat
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
addTmCt :: Delta -> TmCt -> MaybeT DsM Delta
addTmCt delta (TmVarCt x y)            = addVarCt delta x y
addTmCt delta (TmCoreCt x e)           = addCoreCt delta x e
addTmCt delta (TmConCt x con tvs args) = addConCt delta x con tvs args
addTmCt delta (TmNotConCt x con)       = addNotConCt delta x con
addTmCt delta (TmBotCt x)              = addBotCt delta x
addTmCt delta (TmNotBotCt x)           = addNotBotCt delta x

-- | Adds the constraint @x ~ ⊥@, e.g. that evaluation of a particular 'Id' @x@
-- surely diverges.
--
-- Only that's a lie, because we don't currently preserve the fact in 'Delta'
-- after we checked compatibility. See Note [Preserving TmBotCt]
addBotCt :: Delta -> Id -> MaybeT DsM Delta
addBotCt delta x
  | canDiverge delta x = pure delta
  | otherwise          = mzero

{- Note [Preserving TmBotCt]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Whenever we add a new constraint to 'Delta' via 'addTmCt', we want to check it
for compatibility with existing constraints in the modeled indert set and then
add it the constraint itself to the inert set.
For a 'TmBotCt' @x ~ ⊥@ we don't actually add it to the inert set after checking
it for compatibility with 'Delta'.
And that is fine in the context of the patter-match checking algorithm!
Whenever we add a 'TmBotCt' (we only do so for checking divergence of bang
patterns and strict constructor matches), we don't add any more constraints to
the inert set afterwards, so we don't need to preserve it.
-}

-- | Record a @x ~/ K@ constraint, e.g. that a particular 'Id' @x@ can't
-- take the shape of a 'PmAltCon' @K@ in the 'Delta' and return @Nothing@ if
-- that leads to a contradiction.
-- See Note [TmState invariants].
addNotConCt :: Delta -> Id -> PmAltCon -> MaybeT DsM Delta
addNotConCt delta@MkDelta{ delta_tm_st = TmSt env reps } x nalt = do
  vi@(VI _ pos neg pm) <- lift (initLookupVarInfo delta x)
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
  let vi_ext = vi{ vi_neg = neg' }
  -- 3. Make sure there's at least one other possible constructor
  vi' <- case nalt of
    PmAltConLike cl
      -> MaybeT (ensureInhabited delta vi_ext{ vi_cache = markMatched cl pm })
    _ -> pure vi_ext
  pure delta{ delta_tm_st = TmSt (setEntrySDIE env x vi') reps }

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
-- See Note [Pattern synonym result type] in GHC.Core.PatSyn.
guessConLikeUnivTyArgsFromResTy :: FamInstEnvs -> Type -> ConLike -> Maybe [Type]
guessConLikeUnivTyArgsFromResTy env res_ty (RealDataCon _) = do
  (tc, tc_args) <- splitTyConApp_maybe res_ty
  -- Consider data families: In case of a DataCon, we need to translate to
  -- the representation TyCon. For PatSyns, they are relative to the data
  -- family TyCon, so we don't need to translate them.
  let (_, tc_args', _) = tcLookupDataFamInst env tc tc_args
  Just tc_args'
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

-- | Adds the constraint @x ~/ ⊥@ to 'Delta'.
--
-- But doesn't really commit to upholding that constraint in the future. This
-- will be rectified in a follow-up patch. The status quo should work good
-- enough for now.
addNotBotCt :: Delta -> Id -> MaybeT DsM Delta
addNotBotCt delta@MkDelta{ delta_tm_st = TmSt env reps } x = do
  vi  <- lift $ initLookupVarInfo delta x
  vi' <- MaybeT $ ensureInhabited delta vi
  -- vi' has probably constructed and then thinned out some PossibleMatches.
  -- We want to cache that work
  pure delta{ delta_tm_st = TmSt (setEntrySDIE env x vi') reps}

ensureInhabited :: Delta -> VarInfo -> DsM (Maybe VarInfo)
   -- Returns (Just vi) if at least one member of each ConLike in the COMPLETE
   -- set satisfies the oracle
   --
   -- Internally uses and updates the ConLikeSets in vi_cache.
   --
   -- NB: Does /not/ filter each ConLikeSet with the oracle; members may
   --     remain that do not statisfy it.  This lazy approach just
   --     avoids doing unnecessary work.
ensureInhabited delta vi = fmap (set_cache vi) <$> test (vi_cache vi) -- This would be much less tedious with lenses
  where
    set_cache vi cache = vi { vi_cache = cache }

    test NoPM    = pure (Just NoPM)
    test (PM ms) = runMaybeT (PM <$> traverse one_set ms)

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
          (_tvs, _vars, ty_cs, strict_arg_tys) <- mkOneConFull arg_tys con
          tracePm "inh_test" (ppr con $$ ppr ty_cs)
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
ensureAllPossibleMatchesInhabited delta@MkDelta{ delta_tm_st = TmSt env reps }
  = runMaybeT (set_tm_cs_env delta <$> traverseSDIE go env)
  where
    set_tm_cs_env delta env = delta{ delta_tm_st = TmSt env reps }
    go vi = MaybeT (ensureInhabited delta vi)

--------------------------------------
-- * Term oracle unification procedure

-- | Adds a @x ~ y@ constraint by trying to unify two 'Id's and record the
-- gained knowledge in 'Delta'.
--
-- Returns @Nothing@ when there's a contradiction. Returns @Just delta@
-- when the constraint was compatible with prior facts, in which case @delta@
-- has integrated the knowledge from the equality constraint.
--
-- See Note [TmState invariants].
addVarCt :: Delta -> Id -> Id -> MaybeT DsM Delta
addVarCt delta@MkDelta{ delta_tm_st = TmSt env _ } x y
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
equate delta@MkDelta{ delta_tm_st = TmSt env reps } x y
  = ASSERT( not (sameRepresentativeSDIE env x y) )
    case (lookupSDIE env x, lookupSDIE env y) of
      (Nothing, _) -> pure (delta{ delta_tm_st = TmSt (setIndirectSDIE env x y) reps })
      (_, Nothing) -> pure (delta{ delta_tm_st = TmSt (setIndirectSDIE env y x) reps })
      -- Merge the info we have for x into the info for y
      (Just vi_x, Just vi_y) -> do
        -- This assert will probably trigger at some point...
        -- We should decide how to break the tie
        MASSERT2( vi_ty vi_x `eqType` vi_ty vi_y, text "Not same type" )
        -- First assume that x and y are in the same equivalence class
        let env_ind = setIndirectSDIE env x y
        -- Then sum up the refinement counters
        let env_refs = setEntrySDIE env_ind y vi_y
        let delta_refs = delta{ delta_tm_st = TmSt env_refs reps }
        -- and then gradually merge every positive fact we have on x into y
        let add_fact delta (cl, tvs, args) = addConCt delta y cl tvs args
        delta_pos <- foldlM add_fact delta_refs (vi_pos vi_x)
        -- Do the same for negative info
        let add_refut delta nalt = addNotConCt delta y nalt
        delta_neg <- foldlM add_refut delta_pos (pmAltConSetElems (vi_neg vi_x))
        -- vi_cache will be updated in addNotConCt, so we are good to
        -- go!
        pure delta_neg

-- | Add a @x ~ K tvs args ts@ constraint.
-- @addConCt x K tvs args ts@ extends the substitution with a solution
-- @x :-> (K, tvs, args)@ if compatible with the negative and positive info we
-- have on @x@, reject (@Nothing@) otherwise.
--
-- See Note [TmState invariants].
addConCt :: Delta -> Id -> PmAltCon -> [TyVar] -> [Id] -> MaybeT DsM Delta
addConCt delta@MkDelta{ delta_tm_st = TmSt env reps } x alt tvs args = do
  VI ty pos neg cache <- lift (initLookupVarInfo delta x)
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
      MaybeT $ addPmCts delta (listToBag ty_cts `unionBags` listToBag tm_cts)
    Nothing -> do
      let pos' = (alt, tvs, args):pos
      pure delta{ delta_tm_st = TmSt (setEntrySDIE env x (VI ty pos' neg cache)) reps}

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
inhabitationCandidates :: Delta -> Type
                       -> DsM (Either Type (TyCon, Id, [InhabitationCandidate]))
inhabitationCandidates MkDelta{ delta_ty_st = ty_st } ty = do
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
will force argument `x`. Hence, `checkMatches` is not sufficient for checking
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
    cand_is_inhabitable :: RecTcChecker -> Delta
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
-- * Providing positive evidence for a Delta

-- | @provideEvidence vs n delta@ returns a list of
-- at most @n@ (but perhaps empty) refinements of @delta@ that instantiate
-- @vs@ to compatible constructor applications or wildcards.
-- Negative information is only retained if literals are involved or when
-- for recursive GADTs.
provideEvidence :: [Id] -> Int -> Delta -> DsM [Delta]
provideEvidence = go
  where
    go _      0 _     = pure []
    go []     _ delta = pure [delta]
    go (x:xs) n delta = do
      tracePm "provideEvidence" (ppr x $$ ppr xs $$ ppr delta $$ ppr n)
      VI _ pos neg _ <- initLookupVarInfo delta x
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
          go (arg_vas ++ xs) n delta
        []
          -- When there are literals involved, just print negative info
          -- instead of listing missed constructors
          | notNull [ l | PmAltLit l <- pmAltConSetElems neg ]
          -> go xs n delta
        [] -> try_instantiate x xs n delta

    -- | Tries to instantiate a variable by possibly following the chain of
    -- newtypes and then instantiating to all ConLikes of the wrapped type's
    -- minimal residual COMPLETE set.
    try_instantiate :: Id -> [Id] -> Int -> Delta -> DsM [Delta]
    -- Convention: x binds the outer constructor in the chain, y the inner one.
    try_instantiate x xs n delta = do
      (_src_ty, dcs, core_ty) <- tntrGuts <$> pmTopNormaliseType (delta_ty_st delta) (idType x)
      let build_newtype (x, delta) (_ty, dc, arg_ty) = do
            y <- lift $ mkPmId arg_ty
            -- Newtypes don't have existentials (yet?!), so passing an empty
            -- list as ex_tvs.
            delta' <- addConCt delta x (PmAltConLike (RealDataCon dc)) [] [y]
            pure (y, delta')
      runMaybeT (foldlM build_newtype (x, delta) dcs) >>= \case
        Nothing -> pure []
        Just (y, newty_delta) -> do
          -- Pick a COMPLETE set and instantiate it (n at max). Take care of ⊥.
          pm     <- vi_cache <$> initLookupVarInfo newty_delta y
          mb_cls <- pickMinimalCompleteSet newty_delta pm
          case uniqDSetToList <$> mb_cls of
            Just cls@(_:_) -> instantiate_cons y core_ty xs n newty_delta cls
            Just [] | not (canDiverge newty_delta y) -> pure []
            -- Either ⊥ is still possible (think Void) or there are no COMPLETE
            -- sets available, so we can assume it's inhabited
            _ -> go xs n newty_delta

    instantiate_cons :: Id -> Type -> [Id] -> Int -> Delta -> [ConLike] -> DsM [Delta]
    instantiate_cons _ _  _  _ _     []       = pure []
    instantiate_cons _ _  _  0 _     _        = pure []
    instantiate_cons _ ty xs n delta _
      -- We don't want to expose users to GHC-specific constructors for Int etc.
      | fmap (isTyConTriviallyInhabited . fst) (splitTyConApp_maybe ty) == Just True
      = go xs n delta
    instantiate_cons x ty xs n delta (cl:cls) = do
      env <- dsGetFamInstEnvs
      case guessConLikeUnivTyArgsFromResTy env ty cl of
        Nothing -> pure [delta] -- No idea idea how to refine this one, so just finish off with a wildcard
        Just arg_tys -> do
          (tvs, arg_vars, new_ty_cs, strict_arg_tys) <- mkOneConFull arg_tys cl
          let new_tm_cs = unitBag (TmConCt x (PmAltConLike cl) tvs arg_vars)
          -- Now check satifiability
          mb_delta <- pmIsSatisfiable delta new_ty_cs new_tm_cs strict_arg_tys
          tracePm "instantiate_cons" (vcat [ ppr x
                                           , ppr (idType x)
                                           , ppr ty
                                           , ppr cl
                                           , ppr arg_tys
                                           , ppr new_tm_cs
                                           , ppr new_ty_cs
                                           , ppr strict_arg_tys
                                           , ppr delta
                                           , ppr mb_delta
                                           , ppr n ])
          con_deltas <- case mb_delta of
            Nothing     -> pure []
            -- NB: We don't prepend arg_vars as we don't have any evidence on
            -- them and we only want to split once on a data type. They are
            -- inhabited, otherwise pmIsSatisfiable would have refuted.
            Just delta' -> go xs n delta'
          other_cons_deltas <- instantiate_cons x ty xs (n - length con_deltas) delta cls
          pure (con_deltas ++ other_cons_deltas)

pickMinimalCompleteSet :: Delta -> PossibleMatches -> DsM (Maybe ConLikeSet)
pickMinimalCompleteSet _ NoPM      = pure Nothing
-- TODO: First prune sets with type info in delta. But this is good enough for
-- now and less costly. See #17386.
pickMinimalCompleteSet _ (PM clss) = do
  tracePm "pickMinimalCompleteSet" (ppr $ NonEmpty.toList clss)
  pure (Just (minimumBy (comparing sizeUniqDSet) clss))

-- | Finds a representant of the semantic equality class of the given @e@.
-- Which is the @x@ of a @let x = e'@ constraint (with @e@ semantically
-- equivalent to @e'@) we encountered earlier, or a fresh identifier if
-- there weren't any such constraints.
representCoreExpr :: Delta -> CoreExpr -> DsM (Delta, Id)
representCoreExpr delta@MkDelta{ delta_tm_st = ts@TmSt{ ts_reps = reps } } e
  | Just rep <- lookupCoreMap reps e = pure (delta, rep)
  | otherwise = do
      rep <- mkPmId (exprType e)
      let reps'  = extendCoreMap reps e rep
      let delta' = delta{ delta_tm_st = ts{ ts_reps = reps' } }
      pure (delta', rep)

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
addCoreCt :: Delta -> Id -> CoreExpr -> MaybeT DsM Delta
addCoreCt delta x e = do
  dflags <- getDynFlags
  let e' = simpleOptExpr dflags e
  lift $ tracePm "addCoreCt" (ppr x $$ ppr e $$ ppr e')
  execStateT (core_expr x e') delta
  where
    -- | Takes apart a 'CoreExpr' and tries to extract as much information about
    -- literals and constructor applications as possible.
    core_expr :: Id -> CoreExpr -> StateT Delta (MaybeT DsM) ()
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
      = modifyT (\delta -> addVarCt delta x y)
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
    equate_with_similar_expr :: Id -> CoreExpr -> StateT Delta (MaybeT DsM) ()
    equate_with_similar_expr x e = do
      rep <- StateT $ \delta -> swap <$> lift (representCoreExpr delta e)
      -- Note that @rep == x@ if we encountered @e@ for the first time.
      modifyT (\delta -> addVarCt delta x rep)

    bind_expr :: CoreExpr -> StateT Delta (MaybeT DsM) Id
    bind_expr e = do
      x <- lift (lift (mkPmId (exprType e)))
      core_expr x e
      pure x

    -- | Look at @let x = K taus theta es@ and generate the following
    -- constraints (assuming universals were dropped from @taus@ before):
    --   1. @a_1 ~ tau_1, ..., a_n ~ tau_n@ for fresh @a_i@
    --   2. @y_1 ~ e_1, ..., y_m ~ e_m@ for fresh @y_i@
    --   3. @x ~ K as ys@
    data_con_app :: Id -> InScopeSet -> DataCon -> [CoreExpr] -> StateT Delta (MaybeT DsM) ()
    data_con_app x in_scope dc args = do
      let dc_ex_tvs              = dataConExTyCoVars dc
          arty                   = dataConSourceArity dc
          (ex_ty_args, val_args) = splitAtList dc_ex_tvs args
          ex_tys                 = map exprToType ex_ty_args
          vis_args               = reverse $ take arty $ reverse val_args
      uniq_supply <- lift $ lift $ getUniqueSupplyM
      let (_, ex_tvs) = cloneTyVarBndrs (mkEmptyTCvSubst in_scope) dc_ex_tvs uniq_supply
          ty_cts      = equateTys (map mkTyVarTy ex_tvs) ex_tys
      -- 1. @a_1 ~ tau_1, ..., a_n ~ tau_n@ for fresh @a_i@. See also #17703
      modifyT $ \delta -> MaybeT $ addPmCts delta (listToBag ty_cts)
      -- 2. @y_1 ~ e_1, ..., y_m ~ e_m@ for fresh @y_i@
      arg_ids <- traverse bind_expr vis_args
      -- 3. @x ~ K as ys@
      pm_alt_con_app x (PmAltConLike (RealDataCon dc)) ex_tvs arg_ids

    -- | Adds a literal constraint, i.e. @x ~ 42@.
    pm_lit :: Id -> PmLit -> StateT Delta (MaybeT DsM) ()
    pm_lit x lit = pm_alt_con_app x (PmAltLit lit) [] []

    -- | Adds the given constructor application as a solution for @x@.
    pm_alt_con_app :: Id -> PmAltCon -> [TyVar] -> [Id] -> StateT Delta (MaybeT DsM) ()
    pm_alt_con_app x con tvs args = modifyT $ \delta -> addConCt delta x con tvs args

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
