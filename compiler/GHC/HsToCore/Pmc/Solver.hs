
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

{-
Authors: George Karachalias <george.karachalias@cs.kuleuven.be>
         Sebastian Graf <sgraf1337@gmail.com>
         Ryan Scott <ryan.gl.scott@gmail.com>
-}

-- | Model refinements type as per the
-- [Lower Your Guards paper](https://dl.acm.org/doi/abs/10.1145/3408989).
-- The main export of the module are the functions 'addPhiCtsNablas' for adding
-- facts to the oracle, 'isInhabited' to check if a refinement type is inhabited
-- and 'generateInhabitingPatterns' to turn a 'Nabla' into a concrete pattern
-- for an equation.
--
-- In terms of the LYG paper, this module is concerned with Sections 3.4, 3.6
-- and 3.7. E.g., it represents refinement types directly as a bunch of
-- normalised refinement types 'Nabla'.

module GHC.HsToCore.Pmc.Solver (

        Nabla, Nablas(..), initNablas,

        PhiCt(..), PhiCts,
        addPhiCtNablas,
        addPhiCtsNablas,

        isInhabited,
        generateInhabitingPatterns, GenerateInhabitingPatternsMode(..)

    ) where

import GHC.Prelude

import GHC.HsToCore.Pmc.Types
import GHC.HsToCore.Pmc.Utils (tracePm, traceWhenFailPm, mkPmId)

import GHC.Driver.Session
import GHC.Driver.Config
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Monad (allM)
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Data.Bag
import GHC.Types.CompleteMatch
import GHC.Types.Unique.Set
import GHC.Types.Unique.DSet
import GHC.Types.Unique.SDFM
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Var      (EvVar)
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Core
import GHC.Core.FVs       (exprFreeVars)
import GHC.Core.Map.Expr
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
import GHC.Core.TyCon.RecWalk
import GHC.Builtin.Names
import GHC.Builtin.Types
import GHC.Builtin.Types.Prim (tYPETyCon)
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Subst (elemTCvSubst)
import GHC.Core.Type
import GHC.Tc.Solver   (tcNormalise, tcCheckGivens, tcCheckWanteds)
import GHC.Core.Unify    (tcMatchTy)
import GHC.Core.Coercion
import GHC.Core.Reduction
import GHC.HsToCore.Monad hiding (foldlM)
import GHC.Tc.Instance.Family
import GHC.Core.FamInstEnv

import Control.Applicative ((<|>))
import Control.Monad (foldM, forM, guard, mzero, when, filterM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Data.Coerce
import Data.Either   (partitionEithers)
import Data.Foldable (foldlM, minimumBy, toList)
import Data.Monoid   (Any(..))
import Data.List     (sortBy, find)
import qualified Data.List.NonEmpty as NE
import Data.Ord      (comparing)

import GHC.Utils.Trace
_ = pprTrace -- to silence unused import warnings

--
-- * Main exports
--

-- | Add a bunch of 'PhiCt's to all the 'Nabla's.
-- Lifts 'addPhiCts' over many 'Nablas'.
addPhiCtsNablas :: Nablas -> PhiCts -> DsM Nablas
addPhiCtsNablas nablas cts = liftNablasM (\d -> addPhiCts d cts) nablas

-- | 'addPmCtsNablas' for a single 'PmCt'.
addPhiCtNablas :: Nablas -> PhiCt -> DsM Nablas
addPhiCtNablas nablas ct = addPhiCtsNablas nablas (unitBag ct)

liftNablasM :: Monad m => (Nabla -> m (Maybe Nabla)) -> Nablas -> m Nablas
liftNablasM f (MkNablas ds) = MkNablas . catBagMaybes <$> (traverse f ds)

-- | Test if any of the 'Nabla's is inhabited. Currently this is pure, because
-- we preserve the invariant that there are no uninhabited 'Nabla's. But that
-- could change in the future, for example by implementing this function in
-- terms of @notNull <$> generateInhabitingPatterns 1 ds@.
isInhabited :: Nablas -> DsM Bool
isInhabited (MkNablas ds) = pure (not (null ds))

-----------------------------------------------
-- * Caching residual COMPLETE sets

-- See Note [Implementation of COMPLETE pragmas]

-- | Update the COMPLETE sets of 'ResidualCompleteMatches', or 'Nothing'
-- if there was no change as per the update function.
updRcm :: (CompleteMatch          -> (Bool, CompleteMatch))
       -> ResidualCompleteMatches -> (Maybe ResidualCompleteMatches)
updRcm f (RCM vanilla pragmas)
  | not any_change = Nothing
  | otherwise      = Just (RCM vanilla' pragmas')
  where
    f' ::  CompleteMatch          -> (Any,  CompleteMatch)
    f' = coerce f
    (chgd, vanilla')  = traverse f' vanilla
    (chgds, pragmas') = traverse (traverse f') pragmas
    any_change        = getAny $ chgd `mappend` chgds

-- | A pseudo-'CompleteMatch' for the vanilla complete set of the given data
-- 'TyCon'.
-- Ex.: @vanillaCompleteMatchTC 'Maybe' ==> Just ("Maybe", {'Just','Nothing'})@
vanillaCompleteMatchTC :: TyCon -> Maybe CompleteMatch
vanillaCompleteMatchTC tc =
  let -- | TYPE acts like an empty data type on the term-level (#14086), but
      -- it is a PrimTyCon, so tyConDataCons_maybe returns Nothing. Hence a
      -- special case.
      mb_dcs | tc == tYPETyCon = Just []
             | otherwise       = tyConDataCons_maybe tc
  in vanillaCompleteMatch . mkUniqDSet . map RealDataCon <$> mb_dcs

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

markMatched :: PmAltCon -> ResidualCompleteMatches -> DsM (Maybe ResidualCompleteMatches)
-- Nothing means the PmAltCon didn't occur in any COMPLETE set.
-- See Note [Shortcutting the inhabitation test] for how this is useful for
-- performance on T17836.
markMatched (PmAltLit _)      _   = pure Nothing -- lits are never part of a COMPLETE set
markMatched (PmAltConLike cl) rcm = do
  rcm' <- addConLikeMatches cl rcm
  let go cm = case lookupUniqDSet (cmConLikes cm) cl of
        Nothing -> (False, cm)
        Just _  -> (True,  cm { cmConLikes = delOneFromUniqDSet (cmConLikes cm) cl })
  pure $ updRcm go rcm'

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

COMPLETE sets are represented internally in GHC as a set of 'ConLike's. For
example, the pragma {-# COMPLETE F, T #-} would be represented as:

  CompleteMatch {F, T} Nothing

What is the Maybe for? Answer: COMPLETE pragmas may optionally specify a
result *type constructor* (cf. T14422):

  class C f where
    foo :: f a -> ()
  pattern P :: C f => f a
  pattern P <- (foo -> ())

  instance C State where
    foo _ = ()
  {-# COMPLETE P :: State #-}

  f :: State a -> ()
  f P = ()
  g :: C f => f a -> ()
  g P = ()

The @:: State@ here means that the types at which the COMPLETE pragma *applies*
is restricted to scrutinee types that are applications of the 'State' TyCon. So
it applies to the match in @f@ but not in @g@ above, resulting in a warning for
the latter but not for the former. The pragma is represented as

  CompleteMatch {P} (Just State)

GHC collects all COMPLETE pragmas from the current module and from imports
into a field in the DsM environment, which can be accessed with
dsGetCompleteMatches from "GHC.HsToCore.Monad".
Currently, COMPLETE pragmas can't be orphans (e.g. at least one ConLike must
also be defined in the module of the pragma) and do not impact recompilation
checking (#18675).

The pattern-match checker will then initialise each variable's 'VarInfo' with
*all* imported COMPLETE sets (in 'GHC.HsToCore.Pmc.Solver.addCompleteMatches'),
well-typed or not, into a 'ResidualCompleteMatches'. The trick is that a
COMPLETE set that is ill-typed for that match variable could never be written by
the user! And we make sure not to report any ill-typed COMPLETE sets when
formatting 'Nabla's for warnings in 'generateInhabitingPatterns'.

A 'ResidualCompleteMatches' is a list of all COMPLETE sets, minus the ConLikes
we know a particular variable can't be (through negative constructor constraints
@x /~ K@ or a failed attempt at instantiating that ConLike during inhabitation
testing). If *any* of the COMPLETE sets become empty, we know that the match
was exhaustive.

We assume that a COMPLETE set does not apply if for one of its
ConLikes we fail to 'matchConLikeResTy' or the
type of the match variable isn't an application of the optional
result type constructor from the pragma. Why don't we simply
prune inapplicable COMPLETE sets from 'ResidualCompleteMatches'?
The answer is that additional type constraints might make more
COMPLETE sets applicable! Example:

  h :: a -> a :~: Boolean -> ()
  h x Refl | T <- x = ()
           | F <- x = ()

If we eagerly prune {F,T} from the residual matches of @x@, then we don't see
that the match in the guards of @h@ is exhaustive, where the COMPLETE set
applies due to refined type information.
-}

-----------------------
-- * Type normalisation

-- | The return value of 'pmTopNormaliseType'
data TopNormaliseTypeResult
  = NormalisedByConstraints Type
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
  -- 'generateInhabitingPatterns'.
  -- For an example, see Note [Type normalisation].

-- | Return the fields of 'HadRedexes'. Returns appropriate defaults in the
-- other cases.
tntrGuts :: TopNormaliseTypeResult -> (Type, [(Type, DataCon, Type)], Type)
tntrGuts (NormalisedByConstraints ty)   = (ty,     [],      ty)
tntrGuts (HadRedexes src_ty ds core_ty) = (src_ty, ds, core_ty)

instance Outputable TopNormaliseTypeResult where
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
pmTopNormaliseType (TySt _ inert) typ = {-# SCC "pmTopNormaliseType" #-} do
  env <- dsGetFamInstEnvs
  tracePm "normalise" (ppr typ)
  -- Before proceeding, we chuck typ into the constraint solver, in case
  -- solving for given equalities may reduce typ some. See
  -- "Wrinkle: local equalities" in Note [Type normalisation].
  typ' <- initTcDsForSolver $ tcNormalise inert typ
  -- Now we look with topNormaliseTypeX through type and data family
  -- applications and newtypes, which tcNormalise does not do.
  -- See also 'TopNormaliseTypeResult'.
  pure $ case topNormaliseTypeX (stepper env) comb typ' of
    Nothing                -> NormalisedByConstraints typ'
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
          Just (HetReduction (Reduction _ rhs) _)
            -> NS_Step rec_nts rhs ((rhs:), id)
          _ -> NS_Done

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
             -> assertPpr (ty_args `lengthIs` tyConArity tc) (ppr ty) True
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

-- | Normalise the given source type to WHNF. If it isn't already in WHNF
-- ('isSourceTypeInWHNF') , it will normalise the type and then try to step
-- through type family applications, but not data family applications or
-- newtypes.
--
-- This is a pretty common case of calling 'pmTopNormaliseType' and it should be
-- efficient.
normaliseSourceTypeWHNF :: TyState -> Type -> DsM Type
normaliseSourceTypeWHNF _     ty | isSourceTypeInWHNF ty = pure ty
normaliseSourceTypeWHNF ty_st ty =
  pmTopNormaliseType ty_st ty >>= \case
    NormalisedByConstraints ty -> pure ty
    HadRedexes ty _ _          -> pure ty

-- | Is the source type in WHNF wrt. 'pmTopNormaliseType'?
--
-- Returns False if the given type is not a TyCon application, or if the TyCon
-- app head is a type family TyCon. (But not for data family TyCons!)
isSourceTypeInWHNF :: Type -> Bool
isSourceTypeInWHNF ty
  | Just (tc, _) <- splitTyConApp_maybe ty = not (isTypeFamilyTyCon tc)
  | otherwise                              = False

{- Note [Type normalisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

Note [Coverage checking Newtype matches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
   to @y@, similar to what 'equate' does, and don't add a @x ≁ ⊥@ constraint.
   This way, the third clause will still be marked as inaccessible RHS instead
   of redundant. This is ensured by calling 'lookupVarInfoNT'.
3. Immediately reject when we find @x ≁ T2@.
Handling of Newtypes is also described in the Appendix of the Lower Your Guards paper,
where you can find the solution in a perhaps more digestible format.
-}

-------------------------
-- * Adding φ constraints
--
-- Figure 7 in the LYG paper.

-- | A high-level pattern-match constraint. Corresponds to φ from Figure 3 of
-- the LYG paper.
data PhiCt
  = PhiTyCt !PredType
  -- ^ A type constraint "T ~ U".
  | PhiCoreCt    !Id !CoreExpr
  -- ^ @PhiCoreCt x e@ encodes "x ~ e", equating @x@ with the 'CoreExpr' @e@.
  | PhiConCt     !Id !PmAltCon ![TyVar] ![PredType] ![Id]
  -- ^ @PhiConCt x K tvs dicts ys@ encodes @K \@tvs dicts ys <- x@, matching @x@
  -- against the 'PmAltCon' application @K \@tvs dicts ys@, binding @tvs@,
  -- @dicts@ and possibly unlifted fields @ys@ in the process.
  -- See Note [Strict fields and variables of unlifted type].
  | PhiNotConCt  !Id !PmAltCon
  -- ^ @PhiNotConCt x K@ encodes "x ≁ K", asserting that @x@ can't be headed
  -- by @K@.
  | PhiBotCt     !Id
  -- ^ @PhiBotCt x@ encodes "x ~ ⊥", equating @x@ to ⊥.
  -- by @K@.
  | PhiNotBotCt !Id
  -- ^ @PhiNotBotCt x y@ encodes "x ≁ ⊥", asserting that @x@ can't be ⊥.

instance Outputable PhiCt where
  ppr (PhiTyCt ty_ct)                 = ppr ty_ct
  ppr (PhiCoreCt x e)                 = ppr x <+> char '~' <+> ppr e
  ppr (PhiConCt x con tvs dicts args) =
    hsep (ppr con : pp_tvs ++ pp_dicts ++ pp_args) <+> text "<-" <+> ppr x
    where
      pp_tvs   = map ((<> char '@') . ppr) tvs
      pp_dicts = map ppr dicts
      pp_args  = map ppr args
  ppr (PhiNotConCt x con)             = ppr x <+> text "≁" <+> ppr con
  ppr (PhiBotCt x)                    = ppr x <+> text "~ ⊥"
  ppr (PhiNotBotCt x)                 = ppr x <+> text "≁ ⊥"

type PhiCts = Bag PhiCt

-- | The fuel for the inhabitation test.
-- See Note [Fuel for the inhabitation test].
initFuel :: Int
initFuel = 4 -- 4 because it's the smallest number that passes f' in T17977b

-- | Adds new constraints to 'Nabla' and returns 'Nothing' if that leads to a
-- contradiction.
--
-- In terms of the paper, this function models the \(⊕_φ\) function in
-- Figure 7 on batches of φ constraints.
addPhiCts :: Nabla -> PhiCts -> DsM (Maybe Nabla)
-- See Note [TmState invariants].
addPhiCts nabla cts = runMaybeT $ do
  let (ty_cts, tm_cts) = partitionPhiCts cts
  nabla' <- addTyCts nabla (listToBag ty_cts)
  nabla'' <- foldlM addPhiTmCt nabla' (listToBag tm_cts)
  inhabitationTest initFuel (nabla_ty_st nabla) nabla''

partitionPhiCts :: PhiCts -> ([PredType], [PhiCt])
partitionPhiCts = partitionEithers . map to_either . toList
  where
    to_either (PhiTyCt pred_ty) = Left pred_ty
    to_either ct                = Right ct

-----------------------------
-- ** Adding type constraints

-- | Adds new type-level constraints by calling out to the type-checker via
-- 'tyOracle'.
addTyCts :: Nabla -> Bag PredType -> MaybeT DsM Nabla
addTyCts nabla@MkNabla{ nabla_ty_st = ty_st } new_ty_cs = do
  ty_st' <- MaybeT (tyOracle ty_st new_ty_cs)
  pure nabla{ nabla_ty_st = ty_st' }

-- | Add some extra type constraints to the 'TyState'; return 'Nothing' if we
-- find a contradiction (e.g. @Int ~ Bool@).
--
-- See Note [Pattern match warnings with insoluble Givens] in GHC.Tc.Solver.
tyOracle :: TyState -> Bag PredType -> DsM (Maybe TyState)
tyOracle ty_st@(TySt n inert) cts
  | isEmptyBag cts
  = pure (Just ty_st)
  | otherwise
  = {-# SCC "tyOracle" #-}
    do { evs <- traverse nameTyCt cts
       ; tracePm "tyOracle" (ppr n $$ ppr cts $$ ppr inert)
       ; mb_new_inert <- initTcDsForSolver $ tcCheckGivens inert evs
         -- return the new inert set and increment the sequence number n
       ; return (TySt (n+1) <$> mb_new_inert) }

-- | Allocates a fresh 'EvVar' name for 'PredTy's.
nameTyCt :: PredType -> DsM EvVar
nameTyCt pred_ty = do
  unique <- getUniqueM
  let occname = mkVarOccFS (fsLit ("pm_"++show unique))
      idname  = mkInternalName unique occname noSrcSpan
  return (mkLocalIdOrCoVar idname Many pred_ty)

-----------------------------
-- ** Adding term constraints

-- | Adds a single higher-level φ constraint by dispatching to the various
-- oracle functions.
--
-- In terms of the paper, this function amounts to the constructor constraint
-- case of \(⊕_φ\) in Figure 7, which "desugars" higher-level φ constraints
-- into lower-level δ constraints. We don't have a data type for δ constraints
-- and call the corresponding oracle function directly instead.
--
-- Precondition: The φ is /not/ a type constraint! These should be handled by
-- 'addTyCts' before, through 'addPhiCts'.
addPhiTmCt :: Nabla -> PhiCt -> MaybeT DsM Nabla
addPhiTmCt _     (PhiTyCt ct)              = pprPanic "addPhiCt:TyCt" (ppr ct) -- See the precondition
addPhiTmCt nabla (PhiCoreCt x e)           = addCoreCt nabla x e
addPhiTmCt nabla (PhiConCt x con tvs dicts args) = do
  -- Case (1) of Note [Strict fields and variables of unlifted type]
  -- PhiConCt correspond to the higher-level φ constraints from the paper with
  -- bindings semantics. It disperses into lower-level δ constraints that the
  -- 'add*Ct' functions correspond to.
  nabla' <- addTyCts nabla (listToBag dicts)
  nabla'' <- addConCt nabla' x con tvs args
  foldlM addNotBotCt nabla'' (filterUnliftedFields con args)
addPhiTmCt nabla (PhiNotConCt x con)       = addNotConCt nabla x con
addPhiTmCt nabla (PhiBotCt x)              = addBotCt nabla x
addPhiTmCt nabla (PhiNotBotCt x)           = addNotBotCt nabla x

filterUnliftedFields :: PmAltCon -> [Id] -> [Id]
filterUnliftedFields con args =
  [ arg | (arg, bang) <- zipEqual "addPhiCt" args (pmAltConImplBangs con)
        , isBanged bang || isUnliftedType (idType arg) ]

-- | Adds the constraint @x ~ ⊥@, e.g. that evaluation of a particular 'Id' @x@
-- surely diverges. Quite similar to 'addConCt', only that it only cares about
-- ⊥.
addBotCt :: Nabla -> Id -> MaybeT DsM Nabla
addBotCt nabla@MkNabla{ nabla_tm_st = ts@TmSt{ ts_facts=env } } x = do
  let (y, vi@VI { vi_bot = bot }) = lookupVarInfoNT (nabla_tm_st nabla) x
  case bot of
    IsNotBot -> mzero      -- There was x ≁ ⊥. Contradiction!
    IsBot    -> pure nabla -- There already is x ~ ⊥. Nothing left to do
    MaybeBot ->            -- We add x ~ ⊥
      -- Case (3) in Note [Strict fields and variables of unlifted type]
      if isUnliftedType (idType x)
        then mzero -- unlifted vars can never be ⊥
        else do
          let vi' = vi{ vi_bot = IsBot }
          pure nabla{ nabla_tm_st = ts{ts_facts = addToUSDFM env y vi' } }

-- | Adds the constraint @x ~/ ⊥@ to 'Nabla'. Quite similar to 'addNotConCt',
-- but only cares for the ⊥ "constructor".
addNotBotCt :: Nabla -> Id -> MaybeT DsM Nabla
addNotBotCt nabla@MkNabla{ nabla_tm_st = ts@TmSt{ts_facts=env} } x = do
  let (y, vi@VI { vi_bot = bot }) = lookupVarInfoNT (nabla_tm_st nabla) x
  case bot of
    IsBot    -> mzero      -- There was x ~ ⊥. Contradiction!
    IsNotBot -> pure nabla -- There already is x ≁ ⊥. Nothing left to do
    MaybeBot -> do         -- We add x ≁ ⊥ and test if x is still inhabited
      -- Mark dirty for a delayed inhabitation test
      let vi' = vi{ vi_bot = IsNotBot}
      pure $ markDirty y
           $ nabla{ nabla_tm_st = ts{ ts_facts = addToUSDFM env y vi' } }

-- | Record a @x ~/ K@ constraint, e.g. that a particular 'Id' @x@ can't
-- take the shape of a 'PmAltCon' @K@ in the 'Nabla' and return @Nothing@ if
-- that leads to a contradiction.
-- See Note [TmState invariants].
addNotConCt :: Nabla -> Id -> PmAltCon -> MaybeT DsM Nabla
addNotConCt _     _ (PmAltConLike (RealDataCon dc))
  | isNewDataCon dc = mzero -- (3) in Note [Coverage checking Newtype matches]
addNotConCt nabla x nalt = do
  (mb_mark_dirty, nabla') <- trvVarInfo go nabla x
  pure $ case mb_mark_dirty of
    Just x  -> markDirty x nabla'
    Nothing -> nabla'
  where
    -- | Update `x`'s 'VarInfo' entry. Fail ('MaybeT') if contradiction,
    -- otherwise return updated entry and `Just x'` if `x` should be marked dirty,
    -- where `x'` is the representative of `x`.
    go :: VarInfo -> MaybeT DsM (Maybe Id, VarInfo)
    go vi@(VI x' pos neg _ rcm) = do
      -- 1. Bail out quickly when nalt contradicts a solution
      let contradicts nalt sol = eqPmAltCon (paca_con sol) nalt == Equal
      guard (not (any (contradicts nalt) pos))
      -- 2. Only record the new fact when it's not already implied by one of the
      -- solutions
      let implies nalt sol = eqPmAltCon (paca_con sol) nalt == Disjoint
      let neg'
            | any (implies nalt) pos = neg
            -- See Note [Completeness checking with required Thetas]
            | hasRequiredTheta nalt  = neg
            | otherwise              = extendPmAltConSet neg nalt
      massert (isPmAltConMatchStrict nalt)
      let vi' = vi{ vi_neg = neg', vi_bot = IsNotBot }
      -- 3. Make sure there's at least one other possible constructor
      mb_rcm' <- lift (markMatched nalt rcm)
      pure $ case mb_rcm' of
        -- If nalt could be removed from a COMPLETE set, we'll get back Just and
        -- have to mark x dirty, by returning Just x'.
        Just rcm' -> (Just x',  vi'{ vi_rcm = rcm' })
        -- Otherwise, nalt didn't occur in any residual COMPLETE set and we
        -- don't have to mark it dirty. So we return Nothing, which in the case
        -- above would have compromised precision.
        -- See Note [Shortcutting the inhabitation test], grep for T17836.
        Nothing   -> (Nothing, vi')

hasRequiredTheta :: PmAltCon -> Bool
hasRequiredTheta (PmAltConLike cl) = notNull req_theta
  where
    (_,_,_,_,req_theta,_,_) = conLikeFullSig cl
hasRequiredTheta _                 = False

-- | Add a @x ~ K tvs args ts@ constraint.
-- @addConCt x K tvs args ts@ extends the substitution with a solution
-- @x :-> (K, tvs, args)@ if compatible with the negative and positive info we
-- have on @x@, reject (@Nothing@) otherwise.
--
-- See Note [TmState invariants].
addConCt :: Nabla -> Id -> PmAltCon -> [TyVar] -> [Id] -> MaybeT DsM Nabla
addConCt nabla@MkNabla{ nabla_tm_st = ts@TmSt{ ts_facts=env } } x alt tvs args = do
  let vi@(VI _ pos neg bot _) = lookupVarInfo ts x
  -- First try to refute with a negative fact
  guard (not (elemPmAltConSet alt neg))
  -- Then see if any of the other solutions (remember: each of them is an
  -- additional refinement of the possible values x could take) indicate a
  -- contradiction
  guard (all ((/= Disjoint) . eqPmAltCon alt . paca_con) pos)
  -- Now we should be good! Add (alt, tvs, args) as a possible solution, or
  -- refine an existing one
  case find ((== Equal) . eqPmAltCon alt . paca_con) pos of
    Just (PACA _con other_tvs other_args) -> do
      -- We must unify existentially bound ty vars and arguments!
      let ty_cts = equateTys (map mkTyVarTy tvs) (map mkTyVarTy other_tvs)
      nabla' <- MaybeT $ addPhiCts nabla (listToBag ty_cts)
      let add_var_ct nabla (a, b) = addVarCt nabla a b
      foldlM add_var_ct nabla' $ zipEqual "addConCt" args other_args
    Nothing -> do
      let pos' = PACA alt tvs args : pos
      let nabla_with bot' =
            nabla{ nabla_tm_st = ts{ts_facts = addToUSDFM env x (vi{vi_pos = pos', vi_bot = bot'})} }
      -- Do (2) in Note [Coverage checking Newtype matches]
      case (alt, args) of
        (PmAltConLike (RealDataCon dc), [y]) | isNewDataCon dc ->
          case bot of
            MaybeBot -> pure (nabla_with MaybeBot)
            IsBot    -> addBotCt (nabla_with MaybeBot) y
            IsNotBot -> addNotBotCt (nabla_with MaybeBot) y
        _ -> assert (isPmAltConMatchStrict alt )
             pure (nabla_with IsNotBot) -- strict match ==> not ⊥

equateTys :: [Type] -> [Type] -> [PhiCt]
equateTys ts us =
  [ PhiTyCt (mkPrimEqPred t u)
  | (t, u) <- zipEqual "equateTys" ts us
  -- The following line filters out trivial Refl constraints, so that we don't
  -- need to initialise the type oracle that often
  , not (eqType t u)
  ]

-- | Adds a @x ~ y@ constraint by merging the two 'VarInfo's and record the
-- gained knowledge in 'Nabla'.
--
-- Returns @Nothing@ when there's a contradiction while merging. Returns @Just
-- nabla@ when the constraint was compatible with prior facts, in which case
-- @nabla@ has integrated the knowledge from the equality constraint.
--
-- See Note [TmState invariants].
addVarCt :: Nabla -> Id -> Id -> MaybeT DsM Nabla
addVarCt nabla@MkNabla{ nabla_tm_st = ts@TmSt{ ts_facts = env } } x y =
  case equateUSDFM env x y of
    (Nothing,   env') -> pure (nabla{ nabla_tm_st = ts{ ts_facts = env' } })
    -- Add the constraints we had for x to y
    (Just vi_x, env') -> do
      let nabla_equated = nabla{ nabla_tm_st = ts{ts_facts = env'} }
      -- and then gradually merge every positive fact we have on x into y
      let add_pos nabla (PACA cl tvs args) = addConCt nabla y cl tvs args
      nabla_pos <- foldlM add_pos nabla_equated (vi_pos vi_x)
      -- Do the same for negative info
      let add_neg nabla nalt = addNotConCt nabla y nalt
      foldlM add_neg nabla_pos (pmAltConSetElems (vi_neg vi_x))

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
      rep <- StateT $ \nabla -> lift (representCoreExpr nabla e)
      -- Note that @rep == x@ if we encountered @e@ for the first time.
      modifyT (\nabla -> addVarCt nabla x rep)

    bind_expr :: CoreExpr -> StateT Nabla (MaybeT DsM) Id
    bind_expr e = do
      x <- lift (lift (mkPmId (exprType e)))
      core_expr x e
      pure x

    -- | Look at @let x = K taus theta es@ and generate the following
    -- constraints (assuming universals were dropped from @taus@ before):
    --   1. @x ≁ ⊥@ if 'K' is not a Newtype constructor.
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
      -- 1. @x ≁ ⊥@ if 'K' is not a Newtype constructor (#18341)
      when (not (isNewDataCon dc)) $
        modifyT $ \nabla -> addNotBotCt nabla x
      -- 2. @a_1 ~ tau_1, ..., a_n ~ tau_n@ for fresh @a_i@. See also #17703
      modifyT $ \nabla -> MaybeT $ addPhiCts nabla (listToBag ty_cts)
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

-- | Finds a representant of the semantic equality class of the given @e@.
-- Which is the @x@ of a @let x = e'@ constraint (with @e@ semantically
-- equivalent to @e'@) we encountered earlier, or a fresh identifier if
-- there weren't any such constraints.
representCoreExpr :: Nabla -> CoreExpr -> DsM (Id, Nabla)
representCoreExpr nabla@MkNabla{ nabla_tm_st = ts@TmSt{ ts_reps = reps } } e
  | Just rep <- lookupCoreMap reps e = pure (rep, nabla)
  | otherwise = do
      rep <- mkPmId (exprType e)
      let reps'  = extendCoreMap reps e rep
      let nabla' = nabla{ nabla_tm_st = ts{ ts_reps = reps' } }
      pure (rep, nabla')

-- | Like 'modify', but with an effectful modifier action
modifyT :: Monad m => (s -> m s) -> StateT s m ()
modifyT f = StateT $ fmap ((,) ()) . f

{- Note [The Pos/Neg invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Invariant applying to each VarInfo: Whenever we have @C @tvs args@ in 'vi_pos',
any entry in 'vi_neg' must be incomparable to C (return Nothing) according to
'eqPmAltCons'. Those entries that are comparable either lead to a refutation
or are redundant. Examples:
* @x ~ Just y@, @x ≁ [Just]@. 'eqPmAltCon' returns @Equal@, so refute.
* @x ~ Nothing@, @x ≁ [Just]@. 'eqPmAltCon' returns @Disjoint@, so negative
  info is redundant and should be discarded.
* @x ~ I# y@, @x ≁ [4,2]@. 'eqPmAltCon' returns @PossiblyOverlap@, so orthogal.
  We keep this info in order to be able to refute a redundant match on i.e. 4
  later on.

This carries over to pattern synonyms and overloaded literals. Say, we have
    pattern Just42 = Just 42
    case Just42 of x
      Nothing -> ()
      Just _  -> ()
Even though we had a solution for the value abstraction called x here in form
of a PatSynCon Just42, this solution is incomparable to both Nothing and
Just. Hence we retain the info in vi_neg, which eventually allows us to detect
the complete pattern match.

The Pos/Neg invariant extends to vi_rcm, which essentially stores positive
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
oracle or doing inhabitation testing) contradictory. This implies a few
invariants:
* Whenever vi_pos overlaps with vi_neg according to 'eqPmAltCon', we refute.
  This is implied by the Note [Pos/Neg invariant].
* Whenever vi_neg subsumes a COMPLETE set, we refute. We consult vi_rcm to
  detect this, but we could just compare whole COMPLETE sets to vi_neg every
  time, if it weren't for performance.

Maintaining these invariants in 'addVarCt' (the core of the term oracle) and
'addNotConCt' is subtle.
* Merging VarInfos. Example: Add the fact @x ~ y@ (see 'equate').
  - (COMPLETE) If we had @x ≁ True@ and @y ≁ False@, then we get
    @x ≁ [True,False]@. This is vacuous by matter of comparing to the built-in
    COMPLETE set, so should refute.
  - (Pos/Neg) If we had @x ≁ True@ and @y ~ True@, we have to refute.
* Adding positive information. Example: Add the fact @x ~ K ys@ (see 'addConCt')
  - (Neg) If we had @x ≁ K@, refute.
  - (Pos) If we had @x ~ K2@, and that contradicts the new solution according to
    'eqPmAltCon' (ex. K2 is [] and K is (:)), then refute.
  - (Refine) If we had @x ≁ K zs@, unify each y with each z in turn.
* Adding negative information. Example: Add the fact @x ≁ Nothing@ (see 'addNotConCt')
  - (Refut) If we have @x ~ K ys@, refute.
  - (COMPLETE) If K=Nothing and we had @x ≁ Just@, then we get
    @x ≁ [Just,Nothing]@. This is vacuous by matter of comparing to the built-in
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

Note [Detecting pattern synonym applications in expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

Note [Completeness checking with required Thetas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

Note [Strict fields and variables of unlifted type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Binders of unlifted type (and strict fields) are unlifted by construction;
they are conceived with an implicit (but delayed checked) @≁⊥@ constraint to
begin with. Hence, desugaring in "GHC.HsToCore.Pmc" is entirely unconcerned
by strict fields, since the forcing happens *before* pattern matching. And
the φ constructor constraints emitted by 'GHC.HsToCore.Pmc.checkGrd' have
complex binding semantics (binding type constraints and unlifted fields), so
unliftedness semantics are entirely confined to the oracle.

These are the moving parts:

  1.  For each strict (or more generally, unlifted) field @s@ of a 'PhiConCt'
      we have to add a @s ≁ ⊥@ constraint in the corresponding case of
      'addPhiTmCt'. Strict fields are devoid of ⊥ by construction, there's
      nothing that a bang pattern would act on. Example from #18341:

        data T = MkT !Int
        f :: T -> ()
        f (MkT  _) | False = () -- inaccessible
        f (MkT !_) | False = () -- redundant, not only inaccessible!
        f _                = ()

      The second clause desugars to @MkT n <- x, !n@. When coverage checked,
      the 'PmCon' @MkT n <- x@ refines the set of values that reach the bang
      pattern with the φ constraints @MkT n <- x@ (Nothing surprising so far).
      Upon that constraint, it disperses into two lower-level δ constraints
      @x ~ MkT n, n ≁ ⊥@ per Equation (3) in Figure 7 of the paper.

      Checking the 'PmBang' @!n@ will then try to add the
      constraint @n ~ ⊥@ to this set to get the diverging set, which is found
      to be empty. Hence the whole clause is detected as redundant, as
      expected.

  2.  Similarly, when performing the 'inhabitationTest', when instantiating a
      constructor we call 'instCon', which generates a higher-level φ
      constructor constraint.

  3.  The preceding points handle unlifted constructor fields, but there also
      are regular binders of unlifted type. We simply fail in 'addBotCt' for
      any binder of unlifted type.
      It would be enough to check for unliftedness once, when the binder comes
      into scope, but we haven't really a way to track that.

  4.  Why not start an 'emptyVarInfo' of unlifted type with @vi_bot = IsNotBot@?
      Because then we'd need to trigger an inhabitation test, because the var
      might actually be void to begin with. But we can't trigger the test from
      'emptyVarInfo'.
      Historically, that is what we did and not doing the test led to #20631,
      where 'addNotBotCt' trivially succeeded, because the 'VarInfo' already
      said 'IsNotBot', implying that a prior inhabitation test succeeded.
-}

-------------------------
-- * Inhabitation testing
--
-- Figure 8 in the LYG paper.

tyStateRefined :: TyState -> TyState -> Bool
-- Makes use of the fact that the two TyStates we compare will never have the
-- same sequence number. It is invalid to call this function when a is not a
-- refinement of b or vice versa!
tyStateRefined a b = ty_st_n a /= ty_st_n b

markDirty :: Id -> Nabla -> Nabla
markDirty x nabla@MkNabla{nabla_tm_st = ts@TmSt{ts_dirty = dirty} } =
  nabla{ nabla_tm_st = ts{ ts_dirty = extendDVarSet dirty x } }

traverseDirty :: Monad m => (VarInfo -> m VarInfo) -> TmState -> m TmState
traverseDirty f ts@TmSt{ts_facts = env, ts_dirty = dirty} =
  go (uniqDSetToList dirty) env
  where
    go []     env  = pure ts{ts_facts=env}
    go (x:xs) !env = do
      vi' <- f (lookupVarInfo ts x)
      go xs (addToUSDFM env x vi')

traverseAll :: Monad m => (VarInfo -> m VarInfo) -> TmState -> m TmState
traverseAll f ts@TmSt{ts_facts = env} = do
  env' <- traverseUSDFM f env
  pure ts{ts_facts = env'}

-- | Makes sure the given 'Nabla' is still inhabited, by trying to instantiate
-- all dirty variables (or all variables when the 'TyState' changed) to concrete
-- inhabitants. It returns a 'Nabla' with the *same* inhabitants, but with some
-- amount of work cached (like failed instantiation attempts) from the test.
--
-- The \(∇ ⊢ x inh\) judgment form in Figure 8 of the LYG paper.
inhabitationTest :: Int -> TyState -> Nabla -> MaybeT DsM Nabla
inhabitationTest 0     _         nabla             = pure nabla
inhabitationTest fuel  old_ty_st nabla@MkNabla{ nabla_tm_st = ts } = {-# SCC "inhabitationTest" #-} do
  -- lift $ tracePm "inhabitation test" $ vcat
  --   [ ppr fuel
  --   , ppr old_ty_st
  --   , ppr nabla
  --   , text "tyStateRefined:" <+> ppr (tyStateRefined old_ty_st (nabla_ty_st nabla))
  --   ]
  -- When type state didn't change, we only need to traverse dirty VarInfos
  ts' <- if tyStateRefined old_ty_st (nabla_ty_st nabla)
            then traverseAll   test_one ts
            else traverseDirty test_one ts
  pure nabla{ nabla_tm_st = ts'{ts_dirty=emptyDVarSet}}
  where
    nabla_not_dirty = nabla{ nabla_tm_st = ts{ts_dirty=emptyDVarSet} }
    test_one :: VarInfo -> MaybeT DsM VarInfo
    test_one vi =
      lift (varNeedsTesting old_ty_st nabla vi) >>= \case
        True -> do
          -- lift $ tracePm "test_one" (ppr vi)
          -- No solution yet and needs testing
          -- We have to test with a Nabla where all dirty bits are cleared
          instantiate (fuel-1) nabla_not_dirty vi
        _ -> pure vi

-- | Checks whether the given 'VarInfo' needs to be tested for inhabitants.
-- Returns `False` when we can skip the inhabitation test, presuming it would
-- say "yes" anyway. See Note [Shortcutting the inhabitation test].
varNeedsTesting :: TyState -> Nabla -> VarInfo -> DsM Bool
varNeedsTesting _         MkNabla{nabla_tm_st=tm_st}     vi
  | elemDVarSet (vi_id vi) (ts_dirty tm_st) = pure True
varNeedsTesting _         _                              vi
  | notNull (vi_pos vi)                     = pure False
varNeedsTesting old_ty_st MkNabla{nabla_ty_st=new_ty_st} _
  -- Same type state => still inhabited
  | not (tyStateRefined old_ty_st new_ty_st) = pure False
varNeedsTesting old_ty_st MkNabla{nabla_ty_st=new_ty_st} vi = do
  -- These normalisations are relatively expensive, but still better than having
  -- to perform a full inhabitation test
  (_, _, old_norm_ty) <- tntrGuts <$> pmTopNormaliseType old_ty_st (idType $ vi_id vi)
  (_, _, new_norm_ty) <- tntrGuts <$> pmTopNormaliseType new_ty_st (idType $ vi_id vi)
  if old_norm_ty `eqType` new_norm_ty
    then pure False
    else pure True

-- | Returns (Just vi) if at least one member of each ConLike in the COMPLETE
-- set satisfies the oracle
--
-- Internally uses and updates the CompleteMatchs in vi_rcm.
--
-- NB: Does /not/ filter each CompleteMatch with the oracle; members may
--     remain that do not statisfy it.  This lazy approach just
--     avoids doing unnecessary work.
instantiate :: Int -> Nabla -> VarInfo -> MaybeT DsM VarInfo
instantiate fuel nabla vi = {-# SCC "instantiate" #-}
  (instBot fuel nabla vi <|> instCompleteSets fuel nabla vi)

-- | The \(⊢_{Bot}\) rule from the paper
instBot :: Int -> Nabla -> VarInfo -> MaybeT DsM VarInfo
instBot _fuel nabla vi = {-# SCC "instBot" #-} do
  _nabla' <- addBotCt nabla (vi_id vi)
  pure vi

addNormalisedTypeMatches :: Nabla -> Id -> DsM (ResidualCompleteMatches, Nabla)
addNormalisedTypeMatches nabla@MkNabla{ nabla_ty_st = ty_st } x
  = trvVarInfo add_matches nabla x
  where
    add_matches vi@VI{ vi_rcm = rcm }
      -- important common case, shaving down allocations of PmSeriesG by -5%
      | isRcmInitialised rcm = pure (rcm, vi)
    add_matches vi@VI{ vi_rcm = rcm } = do
      norm_res_ty <- normaliseSourceTypeWHNF ty_st (idType x)
      env <- dsGetFamInstEnvs
      rcm' <- case splitReprTyConApp_maybe env norm_res_ty of
        Just (rep_tc, _args, _co)  -> addTyConMatches rep_tc rcm
        Nothing                    -> addCompleteMatches rcm
      pure (rcm', vi{ vi_rcm = rcm' })

-- | Does a 'splitTyConApp_maybe' and then tries to look through a data family
-- application to find the representation TyCon, to which the data constructors
-- are attached. Returns the representation TyCon, the TyCon application args
-- and a representational coercion that will be Refl for non-data family apps.
splitReprTyConApp_maybe :: FamInstEnvs -> Type -> Maybe (TyCon, [Type], Coercion)
splitReprTyConApp_maybe env ty =
  uncurry (tcLookupDataFamInst env) <$> splitTyConApp_maybe ty

-- | This is the |-Inst rule from the paper (section 4.5). Tries to
-- find an inhabitant in every complete set by instantiating with one their
-- constructors. If there is any complete set where we can't find an
-- inhabitant, the whole thing is uninhabited. It returns the updated 'VarInfo'
-- where all the attempted ConLike instantiations have been purged from the
-- 'ResidualCompleteMatches', which functions as a cache.
instCompleteSets :: Int -> Nabla -> VarInfo -> MaybeT DsM VarInfo
instCompleteSets fuel nabla vi = {-# SCC "instCompleteSets" #-} do
  let x = vi_id vi
  (rcm, nabla) <- lift (addNormalisedTypeMatches nabla x)
  nabla <- foldM (\nabla cls -> instCompleteSet fuel nabla x cls) nabla (getRcm rcm)
  pure (lookupVarInfo (nabla_tm_st nabla) x)

anyConLikeSolution :: (ConLike -> Bool) -> [PmAltConApp] -> Bool
anyConLikeSolution p = any (go . paca_con)
  where
    go (PmAltConLike cl) = p cl
    go _                 = False

-- | @instCompleteSet fuel nabla x cls@ iterates over @cls@ until it finds
-- the first inhabited ConLike (as per 'instCon'). Any failed instantiation
-- attempts of a ConLike are recorded as negative information in the returned
-- 'Nabla', so that later calls to this function can skip repeatedly fruitless
-- instantiation of that same constructor.
--
-- Note that the returned Nabla is just a different representation of the
-- original Nabla, not a proper refinement! No positive information will be
-- added, only negative information from failed instantiation attempts,
-- entirely as an optimisation.
instCompleteSet :: Int -> Nabla -> Id -> CompleteMatch -> MaybeT DsM Nabla
instCompleteSet fuel nabla x cs
  | anyConLikeSolution (`elementOfUniqDSet` (cmConLikes cs)) (vi_pos vi)
  -- No need to instantiate a constructor of this COMPLETE set if we already
  -- have a solution!
  = pure nabla
  | not (completeMatchAppliesAtType (varType x) cs)
  = pure nabla
  | otherwise
  = {-# SCC "instCompleteSet" #-} go nabla (sorted_candidates cs)
  where
    vi = lookupVarInfo (nabla_tm_st nabla) x

    sorted_candidates :: CompleteMatch -> [ConLike]
    sorted_candidates cm
      -- If there aren't many candidates, we can try to sort them by number of
      -- strict fields, type constraints, etc., so that we are fast in the
      -- common case
      -- (either many simple constructors *or* few "complicated" ones).
      | sizeUniqDSet cs <= 5 = sortBy compareConLikeTestability (uniqDSetToList cs)
      | otherwise            = uniqDSetToList cs
      where cs = cmConLikes cm

    go :: Nabla -> [ConLike] -> MaybeT DsM Nabla
    go _     []         = mzero
    go nabla (RealDataCon dc:_)
      -- See Note [DataCons that are definitely inhabitable]
      -- Recall that dc can't be in vi_neg, because then it would be
      -- deleted from the residual COMPLETE set.
      | isDataConTriviallyInhabited dc
      = pure nabla
    go nabla (con:cons) = do
      let x = vi_id vi
      let recur_not_con = do
            nabla' <- addNotConCt nabla x (PmAltConLike con)
            go nabla' cons
      (nabla <$ instCon fuel nabla x con) -- return the original nabla, not the
                                          -- refined one!
            <|> recur_not_con -- Assume that x can't be con. Encode that fact
                              -- with addNotConCt and recur.

-- | Is this 'DataCon' trivially inhabited, that is, without needing to perform
-- any inhabitation testing because of strict/unlifted fields or type
-- equalities? See Note [DataCons that are definitely inhabitable]
isDataConTriviallyInhabited :: DataCon -> Bool
isDataConTriviallyInhabited dc
  | isTyConTriviallyInhabited (dataConTyCon dc) = True
isDataConTriviallyInhabited dc =
  null (dataConTheta dc) &&         -- (1)
  null (dataConImplBangs dc) &&     -- (2)
  null (dataConUnliftedFieldTys dc) -- (3)

dataConUnliftedFieldTys :: DataCon -> [Type]
dataConUnliftedFieldTys =
  -- A representation-polymorphic field requires an inhabitation test, hence compare to
  -- @Just True@
  filter ((== Just True) . isLiftedType_maybe) . map scaledThing . dataConOrigArgTys

isTyConTriviallyInhabited :: TyCon -> Bool
isTyConTriviallyInhabited tc = elementOfUniqSet (getUnique tc) triviallyInhabitedTyConKeys

-- | All these types are trivially inhabited
triviallyInhabitedTyConKeys :: UniqSet Unique
triviallyInhabitedTyConKeys = mkUniqSet [
    charTyConKey, doubleTyConKey, floatTyConKey,
    intTyConKey, int8TyConKey, int16TyConKey, int32TyConKey, int64TyConKey,
    intPrimTyConKey, int8PrimTyConKey, int16PrimTyConKey, int32PrimTyConKey, int64PrimTyConKey,
    wordTyConKey, word8TyConKey, word16TyConKey, word32TyConKey, word64TyConKey,
    wordPrimTyConKey, word8PrimTyConKey, word16PrimTyConKey, word32PrimTyConKey, word64PrimTyConKey,
    trTyConTyConKey
  ]

compareConLikeTestability :: ConLike -> ConLike -> Ordering
-- We should instantiate DataCons first, because they are likely to occur in
-- multiple COMPLETE sets at once and we might find that multiple COMPLETE sets
-- are inhabitated by instantiating only a single DataCon.
compareConLikeTestability PatSynCon{}     _               = GT
compareConLikeTestability _               PatSynCon{}     = GT
compareConLikeTestability (RealDataCon a) (RealDataCon b) = mconcat
  -- Thetas are most expensive to check, as they might incur a whole new round
  -- of inhabitation testing
  [ comparing (fast_length . dataConTheta)
  -- Unlifted or strict fields only incur an inhabitation test for that
  -- particular field. Still something to avoid.
  , comparing unlifted_or_strict_fields
  ] a b
  where
    fast_length :: [a] -> Int
    fast_length xs = atLength length 6 xs 5 -- @min 6 (length xs)@, but O(1)

    -- An upper bound on the number of strict or unlifted fields. Approximate in
    -- the unlikely bogus case of an unlifted field that has a bang.
    unlifted_or_strict_fields :: DataCon -> Int
    unlifted_or_strict_fields dc = fast_length (dataConImplBangs dc)
                                 + fast_length (dataConUnliftedFieldTys dc)

-- | @instCon fuel nabla (x::match_ty) K@ tries to instantiate @x@ to @K@ by
-- adding the proper constructor constraint.
--
-- See Note [Instantiating a ConLike].
instCon :: Int -> Nabla -> Id -> ConLike -> MaybeT DsM Nabla
instCon fuel nabla@MkNabla{nabla_ty_st = ty_st} x con = {-# SCC "instCon" #-} MaybeT $ do
  let hdr what = "instCon " ++ show fuel ++ " " ++ what
  env <- dsGetFamInstEnvs
  let match_ty = idType x
  tracePm (hdr "{") $
    ppr con <+> text "... <-" <+> ppr x <+> dcolon <+> ppr match_ty
  norm_match_ty <- normaliseSourceTypeWHNF ty_st match_ty
  mb_sigma_univ <- matchConLikeResTy env ty_st norm_match_ty con
  case mb_sigma_univ of
    Just sigma_univ -> do
      let (_univ_tvs, ex_tvs, eq_spec, thetas, _req_theta, field_tys, _con_res_ty)
            = conLikeFullSig con
      -- Following Note [Instantiating a ConLike]:
      -- (1) _req_theta has been tested in 'matchConLikeResTy'
      -- (2) Instantiate fresh existentials
      (sigma_ex, _) <- cloneTyVarBndrs sigma_univ ex_tvs <$> getUniqueSupplyM
      -- (3) Substitute provided constraints bound by the constructor.
      --     These are added to the type oracle as new facts (in a moment)
      let gammas = substTheta sigma_ex (eqSpecPreds eq_spec ++ thetas)
      -- (4) Instantiate fresh term variables as arguments to the constructor
      let field_tys' = substTys sigma_ex $ map scaledThing field_tys
      arg_ids <- mapM mkPmId field_tys'
      tracePm (hdr "(cts)") $ vcat
        [ ppr x <+> dcolon <+> ppr match_ty
        , text "In WHNF:" <+> ppr (isSourceTypeInWHNF match_ty) <+> ppr norm_match_ty
        , ppr con <+> dcolon <+> text "... ->" <+> ppr _con_res_ty
        , ppr (map (\tv -> ppr tv <+> char '↦' <+> ppr (substTyVar sigma_univ tv)) _univ_tvs)
        , ppr gammas
        , ppr (map (\x -> ppr x <+> dcolon <+> ppr (idType x)) arg_ids)
        ]
      -- (5) Finally add the new constructor constraint
      runMaybeT $ do
        -- Case (2) of Note [Strict fields and variables of unlifted type]
        let alt = PmAltConLike con
        let branching_factor = length $ filterUnliftedFields alt arg_ids
        let ct = PhiConCt x alt ex_tvs gammas arg_ids
        nabla1 <- traceWhenFailPm (hdr "(ct unsatisfiable) }") (ppr ct) $
                  addPhiTmCt nabla ct
        -- See Note [Fuel for the inhabitation test]
        let new_fuel
              | branching_factor <= 1 = fuel
              | otherwise             = min fuel 2
        lift $ tracePm (hdr "(ct satisfiable)") $ vcat
          [ ppr ct
          , ppr x <+> dcolon <+> ppr match_ty
          , text "In WHNF:" <+> ppr (isSourceTypeInWHNF match_ty) <+> ppr norm_match_ty
          , ppr con <+> dcolon <+> text "... ->" <+> ppr _con_res_ty
          , ppr (map (\tv -> ppr tv <+> char '↦' <+> ppr (substTyVar sigma_univ tv)) _univ_tvs)
          , ppr gammas
          , ppr (map (\x -> ppr x <+> dcolon <+> ppr (idType x)) arg_ids)
          , ppr branching_factor
          , ppr new_fuel
          ]
        nabla2 <- traceWhenFailPm (hdr "(inh test failed) }") (ppr nabla1) $
                  inhabitationTest new_fuel (nabla_ty_st nabla) nabla1
        lift $ tracePm (hdr "(inh test succeeded) }") (ppr nabla2)
        pure nabla2
    Nothing -> do
      tracePm (hdr "(match_ty not instance of res_ty) }") empty
      pure (Just nabla) -- Matching against match_ty failed. Inhabited!
                        -- See Note [Instantiating a ConLike].

-- | @matchConLikeResTy _ _ ty K@ tries to match @ty@ against the result
-- type of @K@, @res_ty@. It returns a substitution @s@ for @K@'s universal
-- tyvars such that @s(res_ty)@ equals @ty@ if successful.
--
-- Make sure that @ty@ is normalised before.
--
-- See Note [Matching against a ConLike result type].
matchConLikeResTy :: FamInstEnvs -> TyState -> Type -> ConLike -> DsM (Maybe TCvSubst)
matchConLikeResTy env _              ty (RealDataCon dc) = pure $ do
  (rep_tc, tc_args, _co) <- splitReprTyConApp_maybe env ty
  if rep_tc == dataConTyCon dc
    then Just (zipTCvSubst (dataConUnivTyVars dc) tc_args)
    else Nothing
matchConLikeResTy _   (TySt _ inert) ty (PatSynCon ps) = {-# SCC "matchConLikeResTy" #-} runMaybeT $ do
  let (univ_tvs,req_theta,_,_,_,con_res_ty) = patSynSig ps
  subst <- MaybeT $ pure $ tcMatchTy con_res_ty ty
  guard $ all (`elemTCvSubst` subst) univ_tvs -- See the Note about T11336b
  if null req_theta
    then pure subst
    else do
      let req_theta' = substTys subst req_theta
      satisfiable <- lift $ initTcDsForSolver $ tcCheckWanteds inert req_theta'
      if satisfiable
        then pure subst
        else mzero

{- Note [Soundness and completeness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Soundness and completeness of the pattern-match checker depend entirely on the
soundness and completeness of the inhabitation test.

Achieving both soundness and completeness at the same time is undecidable.
See also T17977 and Note [Fuel for the inhabitation test].
Losing soundness would make the algorithm pointless; hence we give up on
completeness, but try to get as close as possible (how close is called
the 'precision' of the algorithm).

Soundness means that you
  1. Can remove clauses flagged as redundant without changing program semantics
     (no false positives).
  2. Can be sure that your program is free of incomplete pattern matches
     when the checker doesn't flag any inexhaustive definitions
     (no false negatives).

A complete algorithm would mean that
  1. When a clause can be deleted without changing program semantics, it will
     be flagged as redundant (no false negatives).
  2. A program that is free of incomplete pattern matches will never have a
     definition be flagged as inexhaustive (no false positives).

Via the LYG algorithm, we reduce both these properties to a property on
the inhabitation test of refinementment types:
  *Soundness*:    If the inhabitation test says "no" for a given refinement type
                  Nabla, then it provably has no inhabitant.
  *Completeness*: If the inhabitation test says "yes" for a given refinement type
                  Nabla, then it provably has an inhabitant.
Our test is sound, but incomplete, so there are instances where we say
"yes" but in fact the Nabla is empty. Which entails false positive exhaustivity
and false negative redundancy warnings, as above.

In summary, we have the following correspondence:

Property     | Exhaustiveness warnings | Redundancy warnings | Inhabitation test |
-------------|-------------------------|---------------------|-------------------|
Soundness    | No false negatives      | No false positives  | Only says "no"    |
             |                         |                     | if there is no    |
             |                         |                     | inhabitant        |
Completeness | No false positives      | No false negatives  | Only says "yes"   |
             |                         |                     | if there is an    |
             |                         |                     | inhabitant        |

Note [Shortcutting the inhabitation test]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generally, we have to re-test a refinement type for inhabitants whenever we
add a new constraint. Often, we can say "no" early, upon trying to add a
contradicting constraint, see Note [The Pos/Neg invariant]. Still, COMPLETE
sets and type evidence are best handled in a delayed fashion, because of
the recursive nature of the test and our fuel-based approach.
But even then there are some cases in which we can skip the full test,
because we are sure that the refinement type is still inhabited. These
conditions are monitored by 'varNeedsTesting'. It returns

- `True` whenever a full inhabitation test is needed
- `False` whenever the test can be skipped, amounting to an inhabitation test
  that says "yes".

According to Note [Soundness and Completeness], this test will never compromise
soundness: The `True` case just forwards to the actual inhabitation test and the
`False` case amounts to an inhabitation test that is trivially sound, because it
never says "no".

Of course, if the returns says `False`, Completeness (and thus Precision) of the
algorithm is affected, but we get to skip costly inhabitation tests. We try to
trade as little Precision as possible against as much Performance as possible.
Here are the tests, in order:

  1. If a variable is dirty (because of a newly added negative term constraint),
     we have to test.
  2. If a variable has positive information, we don't have to test: The
     positive information acts as constructive proof for inhabitation.
  3. If the type state didn't change, there is no need to test.
  4. If the variable's normalised type didn't change, there is no need to test.
  5. Otherwise, we have to test.

Why (1) before (2)?
-------------------
Consider the reverse for (T18960):
  pattern P x = x
  {-# COMPLETE P :: () #-}
  foo = case () of x@(P _) -> ()
This should be exhaustive. But if we say "We know `x` has solution `()`, so it's
inhabited", then we'll get a warning saying that `()` wasn't matched.
But the match on `P` added the new negative information to the uncovered set,
in the process of which we marked `x` as dirty. By giving the dirty flag a
higher priority than positive info, we get to test again and see that `x` is
uninhabited and the match is exhaustive.

But suppose that `P` wasn't mentioned in any COMPLETE set. Then we simply
don't mark `x` as dirty and will emit a warning again (which we would anyway),
without running a superfluous inhabitation test. That speeds up T17836
considerably.

Why (2) before (3) and (4)?
---------------------------
Simply because (2) is more efficient to test than (3) (not by a lot), which
is more efficient to test than (4), which is still more efficient than running
the full inhabitation test (5).

Note [Fuel for the inhabitation test]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Whether or not a type is inhabited is undecidable in general, see also
Note [Soundness and Completeness]. As a result, we can run into infinite
loops in `inhabitationTest`. Therefore, we adopt a fuel-based approach to
prevent that.

Consider the following example:

  data Abyss = MkAbyss !Abyss
  stareIntoTheAbyss :: Abyss -> a
  stareIntoTheAbyss x = case x of {}

In principle, stareIntoTheAbyss is exhaustive, since there is no way to
construct a terminating value using MkAbyss. But this can't be proven by mere
instantiation and requires an inductive argument, which `inhabitationTest`
currently isn't equipped to do.

In order to prevent endless instantiation attempts in @inhabitationTest@, we
use the fuel as an upper bound on such attempts.

The same problem occurs with recursive newtypes, like in the following code:

  newtype Chasm = MkChasm Chasm
  gazeIntoTheChasm :: Chasm -> a
  gazeIntoTheChasm x = case x of {} -- Erroneously warned as non-exhaustive

So this limitation is somewhat understandable.

Note that even with this recursion detection, there is still a possibility that
`inhabitationTest` can run in exponential time in the amount of fuel. Consider
the following data type:

  data T = MkT !T !T !T

If we try to instantiate each of its fields, that will require us to once again
check if `MkT` is inhabitable in each of those three fields, which in turn will
require us to check if `MkT` is inhabitable again... As you can see, the
branching factor adds up quickly, and if the initial fuel is, say,
100, then the inhabiation test will effectively take forever.

To mitigate this, we check the branching factor every time we are about to do
inhabitation testing in 'instCon'. If the branching factor exceeds 1
(i.e., if there is potential for exponential runtime), then we limit the
maximum recursion depth to 1 to mitigate the problem. If the branching factor
is exactly 1 (i.e., we have a linear chain instead of a tree), then it's okay
to stick with a larger maximum recursion depth.

In #17977 we saw that the defaultRecTcMaxBound (100 at the time of writing) was
too large and had detrimental effect on performance of the coverage checker.
Given that we only commit to a best effort anyway, we decided to substantially
decrement the fuel to 4, at the cost of precision in some edge cases
like

  data Nat = Z | S Nat
  data Down :: Nat -> Type where
    Down :: !(Down n) -> Down (S n)
  f :: Down (S (S (S (S (S Z))))) -> ()
  f x = case x of {}

Since the coverage won't bother to instantiate Down 4 levels deep to see that it
is in fact uninhabited, it will emit a inexhaustivity warning for the case.

Note [DataCons that are definitely inhabitable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Another microoptimization applies to data types like this one:

  data S a = S ![a] !T

Even though there is a strict field of type [a], it's quite silly to call
'instCon' on it, since it's "obvious" that it is inhabitable. To make this
intuition formal, we say that a DataCon C is definitely inhabitable (DI) if:

  1. C has no equality constraints (since they might be unsatisfiable)
  2. C has no strict arguments (since they might be uninhabitable)
  3. C has no unlifted argument types (since they might be uninhabitable)

It's relatively cheap to check if a DataCon is DI, so before we call 'instCon'
on a constructor of a COMPLETE set, we filter out all of the DI ones.

This fast path shaves down -7% allocations for PmSeriesG, for example.

Note [Matching against a ConLike result type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given a ConLike

> C :: forall us. R => ... -> res_ty

is a pattern `C ...` compatible with the type `ty`? Clearly that is the case if
`res_ty` /subsumes/ `ty` and the required constraints `R` (strictly a feature of
pattern synonyms) are satisfiable. In that case, 'matchConLikeResTy' returns a
substitution σ over `us` such that `σ(res_ty) == ty`.

It's surprisingly tricky to implement correctly, and works quite different for
DataCons and PatSynCons:

  * For data cons, we look at `ty` and see if it's a TyCon app `T t1 ... tn`.
    If that is the case, we make sure that `C` is a DataCon of `T` and return
    a substitution mapping `C`'s universal tyvars `us` to `t1`...`tn`.

    Wrinkle: Since `T` might be a data family TyCon, we have to look up its
    representation TyCon before we compare to `C`'s TyCon.
    So we use 'splitReprTyConApp_maybe' instead of 'splitTyConApp_maybe'.

  * For pattern synonyms, we directly match `ty` against `res_ty` to get the
    substitution σ. See Note [Pattern synonym result type] in "GHC.Core.PatSyn".

    Fortunately, we don't have to treat data family TyCons specially:
    Pattern synonyms /never/ apply to a data family representation TyCon.
    We do have to consider the required constraints `σ(R)`, though, as we have
    seen in #19475. That is done by solving them as Wanted constraints given the
    inert set of the current type state (which is part of a Nabla's TySt). Since
    spinning up a constraint solver session is costly, we only do so in the rare
    cases that a pattern synonym actually carries any required constraints.

    We can get into the strange situation that not all universal type variables
    `us` occur in `res_ty`. Example from T11336b:

      instance C Proxy where ...                      -- impl uninteresting
      pattern P :: forall f a. C f => f a -> Proxy a  -- impl uninteresting

      fun :: Proxy a -> ()
      fun (P Proxy) = ()
      fun (P Proxy) = () -- ideally detected as redundant

    `f` is a universal type variable and `C f` the required constraint of
    pattern synonym `P`. But `f` doesn't occur in the result type `Proxy a` of
    `P`, so σ will not even have `f` in its in-scope set. It's a bit unclear
    what to do here; we might want to freshen `f` to `f'` and see if we can
    solve `C f'` as a Wanted constraint, which we most likely can't.
    Hence, we simply skip the freshening and declare the match as failed when
    there is a variable like `f`. For the definition of `fun`, that
    means we will not remember that we matched on `P` and thus will
    not detect its second clause as redundant.

    See Note [Pattern synonym result type] in "GHC.Core.PatSyn" for similar
    oddities.

Note [Instantiating a ConLike]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`instCon` implements the the \(Inst\) function from Figure 8 of the LYG paper.

Given the following type of ConLike `K`

> K :: forall us. R => forall es. P => t1 -> ... -> tn -> res_ty

and a variable `x::match_ty`, it tries to find an instantiation
`K ex_tvs gammas arg_ids :: match_ty` (for fresh `arg_ids`) and ultimately adds
a constructor constraint `K ex_tvs gammas arg_ids <- x` to the given Nabla.

As a first step, it tries (via 'matchConLikeResTy') to match `match_ty` against
`res_ty` and checks that that the required constraints @R@ are satisfiable.
See Note [Matching against a ConLike result type].

If matching /fails/, it trivially (and conservatively) reports "inhabited" by
returning the unrefined input Nabla. After all, the match might have failed due
to incomplete type information in Nabla.
(Later type refinement from unpacking GADT constructors might monomorphise
`match_ty` so much that `res_ty` ultimately subsumes it.)

If matching /succeeds/, we get a substitution σ for the (universal)
tyvars `us`. After applying σ, we get

> K @σ(us) :: σ(R) => forall σ(es). σ(P) => σ(t1) -> ... -> σ(tn) -> match_ty

The existentials `es` might still occur in argument types `σ(tn)`, though.
Now 'instCon' performs the following steps:

 1. It drops the required constraints `σ(R)`, as they have already been
    discharged by 'matchConLikeResTy'.
 2. It instantiates fresh binders `es'` for the other type variables `es`
    bound by `K` and adds the mapping to σ to get σ', so that we have

    > K @σ(us) @es' :: σ'(P) => σ'(t1) -> ... -> σ'(tn) -> match_ty

 3. It adds new type constraints from the substituted
    provided constraints @σ'(P)@.
 4. It substitutes and conjures new binders @arg_ids@ for the argument types
    @σ'(t1) ... σ'(tn)@.
 5. It adds a term constraint @K es' σ'(P) arg_ids <- x@, which handles
    the details regarding type constraints and unlifted fields.

And finally the extended 'Nabla' is returned if all the constraints were
compatible.
-}

--------------------------------------
-- * Generating inhabitants of a Nabla
--
-- This is important for warnings. Roughly corresponds to G in Figure 6 of the
-- LYG paper, with a few tweaks for better warning messages.

-- | See Note [Case split inhabiting patterns]
data GenerateInhabitingPatternsMode
  = CaseSplitTopLevel
  | MinimalCover
  deriving (Eq, Show)

instance Outputable GenerateInhabitingPatternsMode where
  ppr = text . show

-- | @generateInhabitingPatterns vs n nabla@ returns a list of at most @n@ (but
-- perhaps empty) refinements of @nabla@ that represent inhabited patterns.
-- Negative information is only retained if literals are involved or for
-- recursive GADTs.
generateInhabitingPatterns :: GenerateInhabitingPatternsMode -> [Id] -> Int -> Nabla -> DsM [Nabla]
-- See Note [Why inhabitationTest doesn't call generateInhabitingPatterns]
generateInhabitingPatterns _    _      0 _     = pure []
generateInhabitingPatterns _    []     _ nabla = pure [nabla]
generateInhabitingPatterns mode (x:xs) n nabla = do
  tracePm "generateInhabitingPatterns" (ppr mode <+> ppr n <+> ppr (x:xs) $$ ppr nabla)
  let VI _ pos neg _ _ = lookupVarInfo (nabla_tm_st nabla) x
  case pos of
    _:_ -> do
      -- Example for multiple solutions (must involve a PatSyn):
      --   f x@(Just _) True | SomePatSyn _ <- x = ...
      -- within the RHS, we know that
      --   * @x ~ Just y@ for some @y@
      --   * @x ~ SomePatSyn z@ for some @z@
      -- and both conditions have to hold /at the same time/. Hence we must
      -- find evidence for @y@ and @z@ that is valid at the same time. These
      -- constitute arg_vas below.
      let arg_vas = concatMap paca_ids pos
      generateInhabitingPatterns mode (arg_vas ++ xs) n nabla
    []
      -- When there are literals involved, just print negative info
      -- instead of listing missed constructors
      | notNull [ l | PmAltLit l <- pmAltConSetElems neg ]
      -> generateInhabitingPatterns mode xs n nabla
      -- When some constructors are impossible or when we don't care for a
      -- minimal cover (Note [Case split inhabiting patterns]), do a case split.
      | not (isEmptyPmAltConSet neg) || mode == CaseSplitTopLevel
      -> try_instantiate x xs n nabla
      -- Else don't do a case split on this var, just print a wildcard.
      -- But try splitting for the remaining vars, of course
      | otherwise
      -> generateInhabitingPatterns mode xs n nabla
  where
    -- | Tries to instantiate a variable by possibly following the chain of
    -- newtypes and then instantiating to all ConLikes of the wrapped type's
    -- minimal residual COMPLETE set.
    try_instantiate :: Id -> [Id] -> Int -> Nabla -> DsM [Nabla]
    -- Convention: x binds the outer constructor in the chain, y the inner one.
    try_instantiate x xs n nabla = do
      (_src_ty, dcs, rep_ty) <- tntrGuts <$> pmTopNormaliseType (nabla_ty_st nabla) (idType x)
      mb_stuff <- runMaybeT $ instantiate_newtype_chain x nabla dcs
      case mb_stuff of
        Nothing -> pure []
        Just (y, newty_nabla) -> do
          let vi = lookupVarInfo (nabla_tm_st newty_nabla) y
          env <- dsGetFamInstEnvs
          rcm <- case splitReprTyConApp_maybe env rep_ty of
            Just (tc, _, _) -> addTyConMatches tc (vi_rcm vi)
            Nothing         -> addCompleteMatches (vi_rcm vi)

          -- Test all COMPLETE sets for inhabitants (n inhs at max). Take care of ⊥.
          clss <- pickApplicableCompleteSets (nabla_ty_st nabla) rep_ty rcm
          case NE.nonEmpty (uniqDSetToList . cmConLikes <$> clss) of
            Nothing ->
              -- No COMPLETE sets ==> inhabited
              generateInhabitingPatterns mode xs n newty_nabla
            Just clss -> do
              -- Try each COMPLETE set, pick the one with the smallest number of
              -- inhabitants
              nablass' <- forM clss (instantiate_cons y rep_ty xs n newty_nabla)
              let nablas' = minimumBy (comparing length) nablass'
              if null nablas' && vi_bot vi /= IsNotBot
                then generateInhabitingPatterns mode xs n newty_nabla -- bot is still possible. Display a wildcard!
                else pure nablas'

    -- | Instantiates a chain of newtypes, beginning at @x@.
    -- Turns @x nabla [T,U,V]@ to @(y, nabla')@, where @nabla'@ we has the fact
    -- @x ~ T (U (V y))@.
    instantiate_newtype_chain :: Id -> Nabla -> [(Type, DataCon, Type)] -> MaybeT DsM (Id, Nabla)
    instantiate_newtype_chain x nabla []                      = pure (x, nabla)
    instantiate_newtype_chain x nabla ((_ty, dc, arg_ty):dcs) = do
      y <- lift $ mkPmId arg_ty
      -- Newtypes don't have existentials (yet?!), so passing an empty
      -- list as ex_tvs.
      nabla' <- addConCt nabla x (PmAltConLike (RealDataCon dc)) [] [y]
      instantiate_newtype_chain y nabla' dcs

    instantiate_cons :: Id -> Type -> [Id] -> Int -> Nabla -> [ConLike] -> DsM [Nabla]
    instantiate_cons _ _  _  _ _     []       = pure []
    instantiate_cons _ _  _  0 _     _        = pure []
    instantiate_cons _ ty xs n nabla _
      -- We don't want to expose users to GHC-specific constructors for Int etc.
      | fmap (isTyConTriviallyInhabited . fst) (splitTyConApp_maybe ty) == Just True
      = generateInhabitingPatterns mode xs n nabla
    instantiate_cons x ty xs n nabla (cl:cls) = do
      -- The following line is where we call out to the inhabitationTest!
      mb_nabla <- runMaybeT $ instCon 4 nabla x cl
      tracePm "instantiate_cons" (vcat [ ppr x <+> dcolon <+> ppr (idType x)
                                       , ppr ty
                                       , ppr cl
                                       , ppr nabla
                                       , ppr mb_nabla
                                       , ppr n ])
      con_nablas <- case mb_nabla of
        Nothing     -> pure []
        -- NB: We don't prepend arg_vars as we don't have any evidence on
        -- them and we only want to split once on a data type. They are
        -- inhabited, otherwise the inhabitation test would have refuted.
        Just nabla' -> generateInhabitingPatterns mode xs n nabla'
      other_cons_nablas <- instantiate_cons x ty xs (n - length con_nablas) nabla cls
      pure (con_nablas ++ other_cons_nablas)

pickApplicableCompleteSets :: TyState -> Type -> ResidualCompleteMatches -> DsM [CompleteMatch]
-- See Note [Implementation of COMPLETE pragmas] on what "applicable" means
pickApplicableCompleteSets ty_st ty rcm = do
  let cl_res_ty_ok :: ConLike -> DsM Bool
      cl_res_ty_ok cl = do
        env <- dsGetFamInstEnvs
        isJust <$> matchConLikeResTy env ty_st ty cl
  let cm_applicable :: CompleteMatch -> DsM Bool
      cm_applicable cm = do
        cls_ok <- allM cl_res_ty_ok (uniqDSetToList (cmConLikes cm))
        let match_ty_ok = completeMatchAppliesAtType ty cm
        pure (cls_ok && match_ty_ok)
  applicable_cms <- filterM cm_applicable (getRcm rcm)
  tracePm "pickApplicableCompleteSets:" $
    vcat
      [ ppr ty
      , ppr rcm
      , ppr applicable_cms
      ]
  return applicable_cms

{- Note [Why inhabitationTest doesn't call generateInhabitingPatterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Why can't we define `inhabitationTest` (IT) in terms of
`generateInhabitingPatterns` (GIP) as

  inhabitationTest nabla = do
    nablas <- lift $ generateInhabitingPatterns all_variables 1 nabla
    guard (notNull nablas)

There are a few technical reasons, like the lack of a fuel-tracking approach
to stay decidable, that could be overcome. But the nail in the coffin is
performance: In order to provide good warning messages, GIP commits to *one*
COMPLETE set, and goes through some hoops to find the minimal one. This implies
it has to look at *all* constructors in the residual COMPLETE matches and see if
they match, if only to filter out ill-typed COMPLETE sets
(see Note [Implementation of COMPLETE pragmas]). That is untractable for an
efficient IT on huge enumerations.

But we still need GIP to produce the Nablas as proxies for
uncovered patterns that we display warnings for. It's fine to pay this price
once at the end, but IT is called far more often than that.

Note [Case split inhabiting patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we have an -XEmptyCase,
```hs
f :: Maybe a -> ()
f x = case x of {}
```
we want to show the different missing patterns {'Just _', 'Nothing'} to the user
instead of an uninformative wildcard pattern '_'.

But when we have a match as part of a function definition, such as
```hs
g :: (Bool, [a]) -> ()
g (_, []) = ()
```
we established in #20642 that we *do not* want to report P1={'(False, (_:_))',
'(True, (_:_))'} when we could just give the singleton set P2={'(_, (_:_))'}!
The latter set M is a "minimal cover" of the space of missing patterns S, in
that

1. The union of M is *exactly* S
2. The size of M is minimal under all such candidates.

In terms of 'g', P2 satisfies both (1) and (2). While P1 satisfies (1), it does
not satisfy (2). The set {'_'} satisfies (2), but it does not satisfy (1) as
it also covers the pattern '(_,[])' which is not part of the space of missing
patterns of 'g', so it would be a bad (too conservative) suggestion.

Note that for -XEmptyCase, we don't want to emit a minimal cover. We arrange
that by passing 'CaseSplitTopLevel' to 'generateInhabitingPatterns'. We detect
the -XEmptyCase case in 'reportWarnings' by looking for 'ReportEmptyCase'.
-}
