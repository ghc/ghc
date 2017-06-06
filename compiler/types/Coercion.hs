{-
(c) The University of Glasgow 2006
-}

{-# LANGUAGE RankNTypes, CPP, MultiWayIf, FlexibleContexts #-}

-- | Module for (a) type kinds and (b) type coercions,
-- as used in System FC. See 'CoreSyn.Expr' for
-- more on System FC and how coercions fit into it.
--
module Coercion (
        -- * Main data type
        Coercion, CoercionN, CoercionR, CoercionP,
        UnivCoProvenance, CoercionHole, LeftOrRight(..),
        Var, CoVar, TyCoVar,
        Role(..), ltRole,

        -- ** Functions over coercions
        coVarTypes, coVarKind, coVarKindsTypesRole, coVarRole,
        coercionType, coercionKind, coercionKinds,
        mkCoercionType,
        coercionRole, coercionKindRole,

        -- ** Constructing coercions
        mkReflCo, mkRepReflCo, mkNomReflCo,
        mkCoVarCo, mkCoVarCos,
        mkAxInstCo, mkUnbranchedAxInstCo,
        mkAxInstRHS, mkUnbranchedAxInstRHS,
        mkAxInstLHS, mkUnbranchedAxInstLHS,
        mkPiCo, mkPiCos, mkCoCast,
        mkSymCo, mkTransCo, mkTransAppCo,
        mkNthCo, mkNthCoRole, mkLRCo,
        mkInstCo, mkAppCo, mkAppCos, mkTyConAppCo, mkFunCo, mkFunCos,
        mkForAllCo, mkForAllCos, mkHomoForAllCos, mkHomoForAllCos_NoRefl,
        mkPhantomCo, mkHomoPhantomCo, toPhantomCo,
        mkUnsafeCo, mkHoleCo, mkUnivCo, mkSubCo,
        mkAxiomInstCo, mkProofIrrelCo,
        downgradeRole, maybeSubCo, mkAxiomRuleCo,
        mkCoherenceCo, mkCoherenceRightCo, mkCoherenceLeftCo,
        mkKindCo, castCoercionKind,

        mkHeteroCoercionType,

        -- ** Decomposition
        instNewTyCon_maybe,

        NormaliseStepper, NormaliseStepResult(..), composeSteppers,
        mapStepResult, unwrapNewTypeStepper,
        topNormaliseNewType_maybe, topNormaliseTypeX,

        decomposeCo, decomposeFunCo, getCoVar_maybe,
        splitTyConAppCo_maybe,
        splitAppCo_maybe,
        splitFunCo_maybe,
        splitForAllCo_maybe,

        nthRole, tyConRolesX, tyConRolesRepresentational, setNominalRole_maybe,

        pickLR,

        isReflCo, isReflCo_maybe, isReflexiveCo, isReflexiveCo_maybe,
        isReflCoVar_maybe,

        -- ** Coercion variables
        mkCoVar, isCoVar, coVarName, setCoVarName, setCoVarUnique,
        isCoVar_maybe,

        -- ** Free variables
        tyCoVarsOfCo, tyCoVarsOfCos, coVarsOfCo,
        tyCoFVsOfCo, tyCoFVsOfCos, tyCoVarsOfCoDSet,
        coercionSize,

        -- ** Substitution
        CvSubstEnv, emptyCvSubstEnv,
        lookupCoVar,
        substCo, substCos, substCoVar, substCoVars, substCoWith,
        substCoVarBndr,
        extendTvSubstAndInScope, getCvSubstEnv,

        -- ** Lifting
        liftCoSubst, liftCoSubstTyVar, liftCoSubstWith, liftCoSubstWithEx,
        emptyLiftingContext, extendLiftingContext,
        liftCoSubstVarBndrCallback, isMappedByLC,

        mkSubstLiftingContext, zapLiftingContext,
        substForAllCoBndrCallbackLC, lcTCvSubst, lcInScopeSet,

        LiftCoEnv, LiftingContext(..), liftEnvSubstLeft, liftEnvSubstRight,
        substRightCo, substLeftCo, swapLiftCoEnv, lcSubstLeft, lcSubstRight,

        -- ** Comparison
        eqCoercion, eqCoercionX,

        -- ** Forcing evaluation of coercions
        seqCo,

        -- * Pretty-printing
        pprCo, pprParendCo,
        pprCoAxiom, pprCoAxBranch, pprCoAxBranchHdr,

        -- * Tidying
        tidyCo, tidyCos,

        -- * Other
        promoteCoercion
       ) where

#include "HsVersions.h"

import TyCoRep
import Type
import TyCon
import CoAxiom
import Var
import VarEnv
import Name hiding ( varName )
import Util
import BasicTypes
import Outputable
import Unique
import Pair
import SrcLoc
import PrelNames
import TysPrim          ( eqPhantPrimTyCon )
import ListSetOps
import Maybes
import UniqFM

import Control.Monad (foldM)
import Control.Arrow ( first )
import Data.Function ( on )

{-
%************************************************************************
%*                                                                      *
     -- The coercion arguments always *precisely* saturate
     -- arity of (that branch of) the CoAxiom.  If there are
     -- any left over, we use AppCo.  See
     -- See [Coercion axioms applied to coercions] in TyCoRep

\subsection{Coercion variables}
%*                                                                      *
%************************************************************************
-}

coVarName :: CoVar -> Name
coVarName = varName

setCoVarUnique :: CoVar -> Unique -> CoVar
setCoVarUnique = setVarUnique

setCoVarName :: CoVar -> Name -> CoVar
setCoVarName   = setVarName

{-
%************************************************************************
%*                                                                      *
                   Pretty-printing CoAxioms
%*                                                                      *
%************************************************************************

Defined here to avoid module loops. CoAxiom is loaded very early on.

-}

pprCoAxiom :: CoAxiom br -> SDoc
pprCoAxiom ax@(CoAxiom { co_ax_branches = branches })
  = hang (text "axiom" <+> ppr ax <+> dcolon)
       2 (vcat (map (ppr_co_ax_branch (const pprType) ax) $ fromBranches branches))

pprCoAxBranch :: CoAxiom br -> CoAxBranch -> SDoc
pprCoAxBranch = ppr_co_ax_branch pprRhs
  where
    pprRhs fam_tc rhs
      | Just (tycon, _) <- splitTyConApp_maybe rhs
      , isDataFamilyTyCon fam_tc
      = pprDataCons tycon

      | otherwise
      = ppr rhs

pprCoAxBranchHdr :: CoAxiom br -> BranchIndex -> SDoc
pprCoAxBranchHdr ax index = pprCoAxBranch ax (coAxiomNthBranch ax index)

ppr_co_ax_branch :: (TyCon -> Type -> SDoc) -> CoAxiom br -> CoAxBranch -> SDoc
ppr_co_ax_branch ppr_rhs
              (CoAxiom { co_ax_tc = fam_tc, co_ax_name = name })
              (CoAxBranch { cab_tvs = tvs
                          , cab_cvs = cvs
                          , cab_lhs = lhs
                          , cab_rhs = rhs
                          , cab_loc = loc })
  = foldr1 (flip hangNotEmpty 2)
        [ pprUserForAll (mkTyVarBinders Inferred (tvs ++ cvs))
        , pprTypeApp fam_tc lhs <+> equals <+> ppr_rhs fam_tc rhs
        , text "-- Defined" <+> pprLoc loc ]
  where
        pprLoc loc
          | isGoodSrcSpan loc
          = text "at" <+> ppr (srcSpanStart loc)

          | otherwise
          = text "in" <+>
              quotes (ppr (nameModule name))

{-
%************************************************************************
%*                                                                      *
        Destructing coercions
%*                                                                      *
%************************************************************************

Note [Function coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~
Remember that
  (->) :: forall r1 r2. TYPE r1 -> TYPE r2 -> TYPE LiftedRep

Hence
  FunCo r co1 co2 :: (s1->t1) ~r (s2->t2)
is short for
  TyConAppCo (->) co_rep1 co_rep2 co1 co2
where co_rep1, co_rep2 are the coercions on the representations.
-}


-- | This breaks a 'Coercion' with type @T A B C ~ T D E F@ into
-- a list of 'Coercion's of kinds @A ~ D@, @B ~ E@ and @E ~ F@. Hence:
--
-- > decomposeCo 3 c = [nth 0 c, nth 1 c, nth 2 c]
decomposeCo :: Arity -> Coercion -> [Coercion]
decomposeCo arity co
  = [mkNthCo n co | n <- [0..(arity-1)] ]
           -- Remember, Nth is zero-indexed

decomposeFunCo :: Coercion -> (Coercion, Coercion)
-- Expects co :: (s1 -> t1) ~ (s2 -> t2)
-- Returns (co1 :: s1~s2, co2 :: t1~t2)
-- See Note [Function coercions] for the "2" and "3"
decomposeFunCo co = ASSERT2( all_ok, ppr co )
                    (mkNthCo 2 co, mkNthCo 3 co)
  where
    Pair s1t1 s2t2 = coercionKind co
    all_ok = isFunTy s1t1 && isFunTy s2t2

-- | Attempts to obtain the type variable underlying a 'Coercion'
getCoVar_maybe :: Coercion -> Maybe CoVar
getCoVar_maybe (CoVarCo cv) = Just cv
getCoVar_maybe _            = Nothing

-- | Attempts to tease a coercion apart into a type constructor and the application
-- of a number of coercion arguments to that constructor
splitTyConAppCo_maybe :: Coercion -> Maybe (TyCon, [Coercion])
splitTyConAppCo_maybe (Refl r ty)
  = do { (tc, tys) <- splitTyConApp_maybe ty
       ; let args = zipWith mkReflCo (tyConRolesX r tc) tys
       ; return (tc, args) }
splitTyConAppCo_maybe (TyConAppCo _ tc cos) = Just (tc, cos)
splitTyConAppCo_maybe (FunCo _ arg res)     = Just (funTyCon, cos)
  where cos = [mkRuntimeRepCo arg, mkRuntimeRepCo res, arg, res]
splitTyConAppCo_maybe _                     = Nothing

-- first result has role equal to input; third result is Nominal
splitAppCo_maybe :: Coercion -> Maybe (Coercion, Coercion)
-- ^ Attempt to take a coercion application apart.
splitAppCo_maybe (AppCo co arg) = Just (co, arg)
splitAppCo_maybe (TyConAppCo r tc args)
  | mightBeUnsaturatedTyCon tc || args `lengthExceeds` tyConArity tc
    -- Never create unsaturated type family apps!
  , Just (args', arg') <- snocView args
  , Just arg'' <- setNominalRole_maybe arg'
  = Just ( mkTyConAppCo r tc args', arg'' )
       -- Use mkTyConAppCo to preserve the invariant
       --  that identity coercions are always represented by Refl

splitAppCo_maybe (Refl r ty)
  | Just (ty1, ty2) <- splitAppTy_maybe ty
  = Just (mkReflCo r ty1, mkNomReflCo ty2)
splitAppCo_maybe _ = Nothing

splitFunCo_maybe :: Coercion -> Maybe (Coercion, Coercion)
splitFunCo_maybe (FunCo _ arg res) = Just (arg, res)
splitFunCo_maybe _ = Nothing

splitForAllCo_maybe :: Coercion -> Maybe (TyVar, Coercion, Coercion)
splitForAllCo_maybe (ForAllCo tv k_co co) = Just (tv, k_co, co)
splitForAllCo_maybe _                     = Nothing

-------------------------------------------------------
-- and some coercion kind stuff

coVarTypes :: CoVar -> Pair Type
coVarTypes cv
  | (_, _, ty1, ty2, _) <- coVarKindsTypesRole cv
  = Pair ty1 ty2

coVarKindsTypesRole :: CoVar -> (Kind,Kind,Type,Type,Role)
coVarKindsTypesRole cv
 | Just (tc, [k1,k2,ty1,ty2]) <- splitTyConApp_maybe (varType cv)
 = let role
         | tc `hasKey` eqPrimTyConKey     = Nominal
         | tc `hasKey` eqReprPrimTyConKey = Representational
         | otherwise                      = panic "coVarKindsTypesRole"
   in (k1,k2,ty1,ty2,role)
 | otherwise = pprPanic "coVarKindsTypesRole, non coercion variable"
                        (ppr cv $$ ppr (varType cv))

coVarKind :: CoVar -> Type
coVarKind cv
  = ASSERT( isCoVar cv )
    varType cv

coVarRole :: CoVar -> Role
coVarRole cv
  | tc `hasKey` eqPrimTyConKey
  = Nominal
  | tc `hasKey` eqReprPrimTyConKey
  = Representational
  | otherwise
  = pprPanic "coVarRole: unknown tycon" (ppr cv <+> dcolon <+> ppr (varType cv))

  where
    tc = case tyConAppTyCon_maybe (varType cv) of
           Just tc0 -> tc0
           Nothing  -> pprPanic "coVarRole: not tyconapp" (ppr cv)

-- | Makes a coercion type from two types: the types whose equality
-- is proven by the relevant 'Coercion'
mkCoercionType :: Role -> Type -> Type -> Type
mkCoercionType Nominal          = mkPrimEqPred
mkCoercionType Representational = mkReprPrimEqPred
mkCoercionType Phantom          = \ty1 ty2 ->
  let ki1 = typeKind ty1
      ki2 = typeKind ty2
  in
  TyConApp eqPhantPrimTyCon [ki1, ki2, ty1, ty2]

mkHeteroCoercionType :: Role -> Kind -> Kind -> Type -> Type -> Type
mkHeteroCoercionType Nominal          = mkHeteroPrimEqPred
mkHeteroCoercionType Representational = mkHeteroReprPrimEqPred
mkHeteroCoercionType Phantom          = panic "mkHeteroCoercionType"

-- | Given a coercion @co1 :: (a :: TYPE r1) ~ (b :: TYPE r2)@,
-- produce a coercion @rep_co :: r1 ~ r2@.
mkRuntimeRepCo :: HasDebugCallStack => Coercion -> Coercion
mkRuntimeRepCo co
  = mkNthCo 0 kind_co
  where
    kind_co = mkKindCo co  -- kind_co :: TYPE r1 ~ TYPE r2
                           -- (up to silliness with Constraint)

isReflCoVar_maybe :: CoVar -> Maybe Coercion
-- If cv :: t~t then isReflCoVar_maybe cv = Just (Refl t)
isReflCoVar_maybe cv
  | Pair ty1 ty2 <- coVarTypes cv
  , ty1 `eqType` ty2
  = Just (Refl (coVarRole cv) ty1)
  | otherwise
  = Nothing

-- | Tests if this coercion is obviously reflexive. Guaranteed to work
-- very quickly. Sometimes a coercion can be reflexive, but not obviously
-- so. c.f. 'isReflexiveCo'
isReflCo :: Coercion -> Bool
isReflCo (Refl {}) = True
isReflCo _         = False

-- | Returns the type coerced if this coercion is reflexive. Guaranteed
-- to work very quickly. Sometimes a coercion can be reflexive, but not
-- obviously so. c.f. 'isReflexiveCo_maybe'
isReflCo_maybe :: Coercion -> Maybe (Type, Role)
isReflCo_maybe (Refl r ty) = Just (ty, r)
isReflCo_maybe _           = Nothing

-- | Slowly checks if the coercion is reflexive. Don't call this in a loop,
-- as it walks over the entire coercion.
isReflexiveCo :: Coercion -> Bool
isReflexiveCo = isJust . isReflexiveCo_maybe

-- | Extracts the coerced type from a reflexive coercion. This potentially
-- walks over the entire coercion, so avoid doing this in a loop.
isReflexiveCo_maybe :: Coercion -> Maybe (Type, Role)
isReflexiveCo_maybe (Refl r ty) = Just (ty, r)
isReflexiveCo_maybe co
  | ty1 `eqType` ty2
  = Just (ty1, r)
  | otherwise
  = Nothing
  where (Pair ty1 ty2, r) = coercionKindRole co

{-
%************************************************************************
%*                                                                      *
            Building coercions
%*                                                                      *
%************************************************************************

These "smart constructors" maintain the invariants listed in the definition
of Coercion, and they perform very basic optimizations.

Note [Role twiddling functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are a plethora of functions for twiddling roles:

mkSubCo: Requires a nominal input coercion and always produces a
representational output. This is used when you (the programmer) are sure you
know exactly that role you have and what you want.

downgradeRole_maybe: This function takes both the input role and the output role
as parameters. (The *output* role comes first!) It can only *downgrade* a
role -- that is, change it from N to R or P, or from R to P. This one-way
behavior is why there is the "_maybe". If an upgrade is requested, this
function produces Nothing. This is used when you need to change the role of a
coercion, but you're not sure (as you're writing the code) of which roles are
involved.

This function could have been written using coercionRole to ascertain the role
of the input. But, that function is recursive, and the caller of downgradeRole_maybe
often knows the input role. So, this is more efficient.

downgradeRole: This is just like downgradeRole_maybe, but it panics if the
conversion isn't a downgrade.

setNominalRole_maybe: This is the only function that can *upgrade* a coercion.
The result (if it exists) is always Nominal. The input can be at any role. It
works on a "best effort" basis, as it should never be strictly necessary to
upgrade a coercion during compilation. It is currently only used within GHC in
splitAppCo_maybe. In order to be a proper inverse of mkAppCo, the second
coercion that splitAppCo_maybe returns must be nominal. But, it's conceivable
that splitAppCo_maybe is operating over a TyConAppCo that uses a
representational coercion. Hence the need for setNominalRole_maybe.
splitAppCo_maybe, in turn, is used only within coercion optimization -- thus,
it is not absolutely critical that setNominalRole_maybe be complete.

Note that setNominalRole_maybe will never upgrade a phantom UnivCo. Phantom
UnivCos are perfectly type-safe, whereas representational and nominal ones are
not. Indeed, `unsafeCoerce` is implemented via a representational UnivCo.
(Nominal ones are no worse than representational ones, so this function *will*
change a UnivCo Representational to a UnivCo Nominal.)

Conal Elliott also came across a need for this function while working with the
GHC API, as he was decomposing Core casts. The Core casts use representational
coercions, as they must, but his use case required nominal coercions (he was
building a GADT). So, that's why this function is exported from this module.

One might ask: shouldn't downgradeRole_maybe just use setNominalRole_maybe as
appropriate? I (Richard E.) have decided not to do this, because upgrading a
role is bizarre and a caller should have to ask for this behavior explicitly.

Note [mkTransAppCo]
~~~~~~~~~~~~~~~~~~~
Suppose we have

  co1 :: a ~R Maybe
  co2 :: b ~R Int

and we want

  co3 :: a b ~R Maybe Int

This seems sensible enough. But, we can't let (co3 = co1 co2), because
that's ill-roled! Note that mkAppCo requires a *nominal* second coercion.

The way around this is to use transitivity:

  co3 = (co1 <b>_N) ; (Maybe co2) :: a b ~R Maybe Int

Or, it's possible everything is the other way around:

  co1' :: Maybe ~R a
  co2' :: Int   ~R b

and we want

  co3' :: Maybe Int ~R a b

then

  co3' = (Maybe co2') ; (co1' <b>_N)

This is exactly what `mkTransAppCo` builds for us. Information for all
the arguments tends to be to hand at call sites, so it's quicker than
using, say, coercionKind.

-}

mkReflCo :: Role -> Type -> Coercion
mkReflCo r ty
  = Refl r ty

-- | Make a representational reflexive coercion
mkRepReflCo :: Type -> Coercion
mkRepReflCo = mkReflCo Representational

-- | Make a nominal reflexive coercion
mkNomReflCo :: Type -> Coercion
mkNomReflCo = mkReflCo Nominal

-- | Apply a type constructor to a list of coercions. It is the
-- caller's responsibility to get the roles correct on argument coercions.
mkTyConAppCo :: HasDebugCallStack => Role -> TyCon -> [Coercion] -> Coercion
mkTyConAppCo r tc cos
  | tc `hasKey` funTyConKey
  , [_rep1, _rep2, co1, co2] <- cos   -- See Note [Function coercions]
  = -- (a :: TYPE ra) -> (b :: TYPE rb)  ~  (c :: TYPE rc) -> (d :: TYPE rd)
    -- rep1 :: ra  ~  rc        rep2 :: rb  ~  rd
    -- co1  :: a   ~  c         co2  :: b   ~  d
    mkFunCo r co1 co2

               -- Expand type synonyms
  | Just (tv_co_prs, rhs_ty, leftover_cos) <- expandSynTyCon_maybe tc cos
  = mkAppCos (liftCoSubst r (mkLiftingContext tv_co_prs) rhs_ty) leftover_cos

  | Just tys_roles <- traverse isReflCo_maybe cos
  = Refl r (mkTyConApp tc (map fst tys_roles))    -- See Note [Refl invariant]

  | otherwise = TyConAppCo r tc cos

-- | Build a function 'Coercion' from two other 'Coercion's. That is,
-- given @co1 :: a ~ b@ and @co2 :: x ~ y@ produce @co :: (a -> x) ~ (b -> y)@.
mkFunCo :: Role -> Coercion -> Coercion -> Coercion
mkFunCo r co1 co2
    -- See Note [Refl invariant]
  | Just (ty1, _) <- isReflCo_maybe co1
  , Just (ty2, _) <- isReflCo_maybe co2
  = Refl r (mkFunTy ty1 ty2)
  | otherwise = FunCo r co1 co2

-- | Make nested function 'Coercion's
mkFunCos :: Role -> [Coercion] -> Coercion -> Coercion
mkFunCos r cos res_co = foldr (mkFunCo r) res_co cos

-- | Apply a 'Coercion' to another 'Coercion'.
-- The second coercion must be Nominal, unless the first is Phantom.
-- If the first is Phantom, then the second can be either Phantom or Nominal.
mkAppCo :: Coercion     -- ^ :: t1 ~r t2
        -> Coercion     -- ^ :: s1 ~N s2, where s1 :: k1, s2 :: k2
        -> Coercion     -- ^ :: t1 s1 ~r t2 s2
mkAppCo (Refl r ty1) arg
  | Just (ty2, _) <- isReflCo_maybe arg
  = Refl r (mkAppTy ty1 ty2)

  | Just (tc, tys) <- splitTyConApp_maybe ty1
    -- Expand type synonyms; a TyConAppCo can't have a type synonym (Trac #9102)
  = mkTyConAppCo r tc (zip_roles (tyConRolesX r tc) tys)
  where
    zip_roles (r1:_)  []            = [downgradeRole r1 Nominal arg]
    zip_roles (r1:rs) (ty1:tys)     = mkReflCo r1 ty1 : zip_roles rs tys
    zip_roles _       _             = panic "zip_roles" -- but the roles are infinite...

mkAppCo (TyConAppCo r tc args) arg
  = case r of
      Nominal          -> mkTyConAppCo Nominal tc (args ++ [arg])
      Representational -> mkTyConAppCo Representational tc (args ++ [arg'])
        where new_role = (tyConRolesRepresentational tc) !! (length args)
              arg'     = downgradeRole new_role Nominal arg
      Phantom          -> mkTyConAppCo Phantom tc (args ++ [toPhantomCo arg])
mkAppCo co arg = AppCo co  arg
-- Note, mkAppCo is careful to maintain invariants regarding
-- where Refl constructors appear; see the comments in the definition
-- of Coercion and the Note [Refl invariant] in TyCoRep.

-- | Applies multiple 'Coercion's to another 'Coercion', from left to right.
-- See also 'mkAppCo'.
mkAppCos :: Coercion
         -> [Coercion]
         -> Coercion
mkAppCos co1 cos = foldl mkAppCo co1 cos

-- | Like `mkAppCo`, but allows the second coercion to be other than
-- nominal. See Note [mkTransAppCo]. Role r3 cannot be more stringent
-- than either r1 or r2.
mkTransAppCo :: Role         -- ^ r1
             -> Coercion     -- ^ co1 :: ty1a ~r1 ty1b
             -> Type         -- ^ ty1a
             -> Type         -- ^ ty1b
             -> Role         -- ^ r2
             -> Coercion     -- ^ co2 :: ty2a ~r2 ty2b
             -> Type         -- ^ ty2a
             -> Type         -- ^ ty2b
             -> Role         -- ^ r3
             -> Coercion     -- ^ :: ty1a ty2a ~r3 ty1b ty2b
mkTransAppCo r1 co1 ty1a ty1b r2 co2 ty2a ty2b r3
-- How incredibly fiddly! Is there a better way??
  = case (r1, r2, r3) of
      (_,                _,                Phantom)
        -> mkPhantomCo kind_co (mkAppTy ty1a ty2a) (mkAppTy ty1b ty2b)
        where -- ty1a :: k1a -> k2a
              -- ty1b :: k1b -> k2b
              -- ty2a :: k1a
              -- ty2b :: k1b
              -- ty1a ty2a :: k2a
              -- ty1b ty2b :: k2b
              kind_co1 = mkKindCo co1        -- :: k1a -> k2a ~N k1b -> k2b
              kind_co  = mkNthCo 1 kind_co1  -- :: k2a ~N k2b

      (_,                _,                Nominal)
        -> ASSERT( r1 == Nominal && r2 == Nominal )
           mkAppCo co1 co2
      (Nominal,          Nominal,          Representational)
        -> mkSubCo (mkAppCo co1 co2)
      (_,                Nominal,          Representational)
        -> ASSERT( r1 == Representational )
           mkAppCo co1 co2
      (Nominal,          Representational, Representational)
        -> go (mkSubCo co1)
      (_               , _,                Representational)
        -> ASSERT( r1 == Representational && r2 == Representational )
           go co1
  where
    go co1_repr
      | Just (tc1b, tys1b) <- splitTyConApp_maybe ty1b
      , nextRole ty1b == r2
      = (mkAppCo co1_repr (mkNomReflCo ty2a)) `mkTransCo`
        (mkTyConAppCo Representational tc1b
           (zipWith mkReflCo (tyConRolesRepresentational tc1b) tys1b
            ++ [co2]))

      | Just (tc1a, tys1a) <- splitTyConApp_maybe ty1a
      , nextRole ty1a == r2
      = (mkTyConAppCo Representational tc1a
           (zipWith mkReflCo (tyConRolesRepresentational tc1a) tys1a
            ++ [co2]))
        `mkTransCo`
        (mkAppCo co1_repr (mkNomReflCo ty2b))

      | otherwise
      = pprPanic "mkTransAppCo" (vcat [ ppr r1, ppr co1, ppr ty1a, ppr ty1b
                                      , ppr r2, ppr co2, ppr ty2a, ppr ty2b
                                      , ppr r3 ])

-- | Make a Coercion from a tyvar, a kind coercion, and a body coercion.
-- The kind of the tyvar should be the left-hand kind of the kind coercion.
mkForAllCo :: TyVar -> Coercion -> Coercion -> Coercion
mkForAllCo tv kind_co co
  | Refl r ty <- co
  , Refl {} <- kind_co
  = Refl r (mkInvForAllTy tv ty)
  | otherwise
  = ForAllCo tv kind_co co

-- | Make nested ForAllCos
mkForAllCos :: [(TyVar, Coercion)] -> Coercion -> Coercion
mkForAllCos bndrs (Refl r ty)
  = let (refls_rev'd, non_refls_rev'd) = span (isReflCo . snd) (reverse bndrs) in
    foldl (flip $ uncurry ForAllCo)
          (Refl r $ mkInvForAllTys (reverse (map fst refls_rev'd)) ty)
          non_refls_rev'd
mkForAllCos bndrs co = foldr (uncurry ForAllCo) co bndrs

-- | Make a Coercion quantified over a type variable;
-- the variable has the same type in both sides of the coercion
mkHomoForAllCos :: [TyVar] -> Coercion -> Coercion
mkHomoForAllCos tvs (Refl r ty)
  = Refl r (mkInvForAllTys tvs ty)
mkHomoForAllCos tvs ty = mkHomoForAllCos_NoRefl tvs ty

-- | Like 'mkHomoForAllCos', but doesn't check if the inner coercion
-- is reflexive.
mkHomoForAllCos_NoRefl :: [TyVar] -> Coercion -> Coercion
mkHomoForAllCos_NoRefl tvs orig_co = foldr go orig_co tvs
  where
    go tv co = ForAllCo tv (mkNomReflCo (tyVarKind tv)) co

mkCoVarCo :: CoVar -> Coercion
-- cv :: s ~# t
-- See Note [mkCoVarCo]
mkCoVarCo cv = CoVarCo cv

mkCoVarCos :: [CoVar] -> [Coercion]
mkCoVarCos = map mkCoVarCo

{- Note [mkCoVarCo]
~~~~~~~~~~~~~~~~~~~
In the past, mkCoVarCo optimised (c :: t~t) to (Refl t).  That is
valid (although see Note [Unbound RULE binders] in Rules), but
it's a relatively expensive test and perhaps better done in
optCoercion.  Not a big deal either way.
-}

-- | Extract a covar, if possible. This check is dirty. Be ashamed
-- of yourself. (It's dirty because it cares about the structure of
-- a coercion, which is morally reprehensible.)
isCoVar_maybe :: Coercion -> Maybe CoVar
isCoVar_maybe (CoVarCo cv) = Just cv
isCoVar_maybe _            = Nothing

mkAxInstCo :: Role -> CoAxiom br -> BranchIndex -> [Type] -> [Coercion]
           -> Coercion
-- mkAxInstCo can legitimately be called over-staturated;
-- i.e. with more type arguments than the coercion requires
mkAxInstCo role ax index tys cos
  | arity == n_tys = downgradeRole role ax_role $
                     mkAxiomInstCo ax_br index (rtys `chkAppend` cos)
  | otherwise      = ASSERT( arity < n_tys )
                     downgradeRole role ax_role $
                     mkAppCos (mkAxiomInstCo ax_br index
                                             (ax_args `chkAppend` cos))
                              leftover_args
  where
    n_tys         = length tys
    ax_br         = toBranchedAxiom ax
    branch        = coAxiomNthBranch ax_br index
    tvs           = coAxBranchTyVars branch
    arity         = length tvs
    arg_roles     = coAxBranchRoles branch
    rtys          = zipWith mkReflCo (arg_roles ++ repeat Nominal) tys
    (ax_args, leftover_args)
                  = splitAt arity rtys
    ax_role       = coAxiomRole ax

-- worker function; just checks to see if it should produce Refl
mkAxiomInstCo :: CoAxiom Branched -> BranchIndex -> [Coercion] -> Coercion
mkAxiomInstCo ax index args
  = ASSERT( args `lengthIs` coAxiomArity ax index )
    AxiomInstCo ax index args

-- to be used only with unbranched axioms
mkUnbranchedAxInstCo :: Role -> CoAxiom Unbranched
                     -> [Type] -> [Coercion] -> Coercion
mkUnbranchedAxInstCo role ax tys cos
  = mkAxInstCo role ax 0 tys cos

mkAxInstRHS :: CoAxiom br -> BranchIndex -> [Type] -> [Coercion] -> Type
-- Instantiate the axiom with specified types,
-- returning the instantiated RHS
-- A companion to mkAxInstCo:
--    mkAxInstRhs ax index tys = snd (coercionKind (mkAxInstCo ax index tys))
mkAxInstRHS ax index tys cos
  = ASSERT( tvs `equalLength` tys1 )
    mkAppTys rhs' tys2
  where
    branch       = coAxiomNthBranch ax index
    tvs          = coAxBranchTyVars branch
    cvs          = coAxBranchCoVars branch
    (tys1, tys2) = splitAtList tvs tys
    rhs'         = substTyWith tvs tys1 $
                   substTyWithCoVars cvs cos $
                   coAxBranchRHS branch

mkUnbranchedAxInstRHS :: CoAxiom Unbranched -> [Type] -> [Coercion] -> Type
mkUnbranchedAxInstRHS ax = mkAxInstRHS ax 0

-- | Return the left-hand type of the axiom, when the axiom is instantiated
-- at the types given.
mkAxInstLHS :: CoAxiom br -> BranchIndex -> [Type] -> [Coercion] -> Type
mkAxInstLHS ax index tys cos
  = ASSERT( tvs `equalLength` tys1 )
    mkTyConApp fam_tc (lhs_tys `chkAppend` tys2)
  where
    branch       = coAxiomNthBranch ax index
    tvs          = coAxBranchTyVars branch
    cvs          = coAxBranchCoVars branch
    (tys1, tys2) = splitAtList tvs tys
    lhs_tys      = substTysWith tvs tys1 $
                   substTysWithCoVars cvs cos $
                   coAxBranchLHS branch
    fam_tc       = coAxiomTyCon ax

-- | Instantiate the left-hand side of an unbranched axiom
mkUnbranchedAxInstLHS :: CoAxiom Unbranched -> [Type] -> [Coercion] -> Type
mkUnbranchedAxInstLHS ax = mkAxInstLHS ax 0

-- | Manufacture an unsafe coercion from thin air.
--   Currently (May 14) this is used only to implement the
--   @unsafeCoerce#@ primitive.  Optimise by pushing
--   down through type constructors.
mkUnsafeCo :: Role -> Type -> Type -> Coercion
mkUnsafeCo role ty1 ty2
  = mkUnivCo UnsafeCoerceProv role ty1 ty2

-- | Make a coercion from a coercion hole
mkHoleCo :: CoercionHole -> Role
         -> Type -> Type -> Coercion
mkHoleCo h r t1 t2 = mkUnivCo (HoleProv h) r t1 t2

-- | Make a universal coercion between two arbitrary types.
mkUnivCo :: UnivCoProvenance
         -> Role       -- ^ role of the built coercion, "r"
         -> Type       -- ^ t1 :: k1
         -> Type       -- ^ t2 :: k2
         -> Coercion   -- ^ :: t1 ~r t2
mkUnivCo prov role ty1 ty2
  | ty1 `eqType` ty2 = Refl role ty1
  | otherwise        = UnivCo prov role ty1 ty2

-- | Create a symmetric version of the given 'Coercion' that asserts
--   equality between the same types but in the other "direction", so
--   a kind of @t1 ~ t2@ becomes the kind @t2 ~ t1@.
mkSymCo :: Coercion -> Coercion

-- Do a few simple optimizations, but don't bother pushing occurrences
-- of symmetry to the leaves; the optimizer will take care of that.
mkSymCo co@(Refl {})              = co
mkSymCo    (SymCo co)             = co
mkSymCo    (SubCo (SymCo co))     = SubCo co
mkSymCo co                        = SymCo co

-- | Create a new 'Coercion' by composing the two given 'Coercion's transitively.
--   (co1 ; co2)
mkTransCo :: Coercion -> Coercion -> Coercion
mkTransCo co1 (Refl {}) = co1
mkTransCo (Refl {}) co2 = co2
mkTransCo co1 co2       = TransCo co1 co2

-- the Role is the desired one. It is the caller's responsibility to make
-- sure this request is reasonable
mkNthCoRole :: Role -> Int -> Coercion -> Coercion
mkNthCoRole role n co
  = downgradeRole role nth_role $ nth_co
  where
    nth_co = mkNthCo n co
    nth_role = coercionRole nth_co

mkNthCo :: Int -> Coercion -> Coercion
mkNthCo 0 (Refl _ ty)
  | Just (tv, _) <- splitForAllTy_maybe ty
  = Refl Nominal (tyVarKind tv)
mkNthCo n (Refl r ty)
  = ASSERT2( ok_tc_app ty n, ppr n $$ ppr ty )
    mkReflCo r' (tyConAppArgN n ty)
  where tc = tyConAppTyCon ty
        r' = nthRole r tc n

        ok_tc_app :: Type -> Int -> Bool
        ok_tc_app ty n
          | Just (_, tys) <- splitTyConApp_maybe ty
          = tys `lengthExceeds` n
          | isForAllTy ty  -- nth:0 pulls out a kind coercion from a hetero forall
          = n == 0
          | otherwise
          = False

mkNthCo 0 (ForAllCo _ kind_co _) = kind_co
  -- If co :: (forall a1:k1. t1) ~ (forall a2:k2. t2)
  -- then (nth 0 co :: k1 ~ k2)

mkNthCo n co@(FunCo _ arg res)
  -- See Note [Function coercions]
  -- If FunCo _ arg_co res_co ::   (s1:TYPE sk1 -> s2:TYPE sk2)
  --                             ~ (t1:TYPE tk1 -> t2:TYPE tk2)
  -- Then we want to behave as if co was
  --    TyConAppCo argk_co resk_co arg_co res_co
  -- where
  --    argk_co :: sk1 ~ tk1  =  mkNthCo 0 (mkKindCo arg_co)
  --    resk_co :: sk2 ~ tk2  =  mkNthCo 0 (mkKindCo res_co)
  --                             i.e. mkRuntimeRepCo
  = case n of
      0 -> mkRuntimeRepCo arg
      1 -> mkRuntimeRepCo res
      2 -> arg
      3 -> res
      _ -> pprPanic "mkNthCo(FunCo)" (ppr n $$ ppr co)

mkNthCo n (TyConAppCo _ _ arg_cos) = arg_cos `getNth` n

mkNthCo n co = NthCo n co

mkLRCo :: LeftOrRight -> Coercion -> Coercion
mkLRCo lr (Refl eq ty) = Refl eq (pickLR lr (splitAppTy ty))
mkLRCo lr co           = LRCo lr co

-- | Instantiates a 'Coercion'.
mkInstCo :: Coercion -> Coercion -> Coercion
mkInstCo (ForAllCo tv _kind_co body_co) (Refl _ arg)
  = substCoWithUnchecked [tv] [arg] body_co
mkInstCo co arg = InstCo co arg

-- This could work harder to produce Refl coercions, but that would be
-- quite inefficient. Seems better not to try.
mkCoherenceCo :: Coercion -> Coercion -> Coercion
mkCoherenceCo co1 (Refl {}) = co1
mkCoherenceCo (CoherenceCo co1 co2) co3
  = CoherenceCo co1 (co2 `mkTransCo` co3)
mkCoherenceCo co1 co2     = CoherenceCo co1 co2

-- | A CoherenceCo c1 c2 applies the coercion c2 to the left-hand type
-- in the kind of c1. This function uses sym to get the coercion on the
-- right-hand type of c1. Thus, if c1 :: s ~ t, then mkCoherenceRightCo c1 c2
-- has the kind (s ~ (t |> c2)) down through type constructors.
-- The second coercion must be representational.
mkCoherenceRightCo :: Coercion -> Coercion -> Coercion
mkCoherenceRightCo c1 c2 = mkSymCo (mkCoherenceCo (mkSymCo c1) c2)

-- | An explicitly directed synonym of mkCoherenceCo. The second
-- coercion must be representational.
mkCoherenceLeftCo :: Coercion -> Coercion -> Coercion
mkCoherenceLeftCo = mkCoherenceCo

infixl 5 `mkCoherenceCo`
infixl 5 `mkCoherenceRightCo`
infixl 5 `mkCoherenceLeftCo`

-- | Given @co :: (a :: k) ~ (b :: k')@ produce @co' :: k ~ k'@.
mkKindCo :: Coercion -> Coercion
mkKindCo (Refl _ ty) = Refl Nominal (typeKind ty)
mkKindCo (UnivCo (PhantomProv h) _ _ _)    = h
mkKindCo (UnivCo (ProofIrrelProv h) _ _ _) = h
mkKindCo co
  | Pair ty1 ty2 <- coercionKind co
       -- generally, calling coercionKind during coercion creation is a bad idea,
       -- as it can lead to exponential behavior. But, we don't have nested mkKindCos,
       -- so it's OK here.
  , let tk1 = typeKind ty1
        tk2 = typeKind ty2
  , tk1 `eqType` tk2
  = Refl Nominal tk1
  | otherwise
  = KindCo co

-- input coercion is Nominal; see also Note [Role twiddling functions]
mkSubCo :: Coercion -> Coercion
mkSubCo (Refl Nominal ty) = Refl Representational ty
mkSubCo (TyConAppCo Nominal tc cos)
  = TyConAppCo Representational tc (applyRoles tc cos)
mkSubCo (FunCo Nominal arg res)
  = FunCo Representational
          (downgradeRole Representational Nominal arg)
          (downgradeRole Representational Nominal res)
mkSubCo co = ASSERT2( coercionRole co == Nominal, ppr co <+> ppr (coercionRole co) )
             SubCo co

-- | Changes a role, but only a downgrade. See Note [Role twiddling functions]
downgradeRole_maybe :: Role   -- ^ desired role
                    -> Role   -- ^ current role
                    -> Coercion -> Maybe Coercion
-- In (downgradeRole_maybe dr cr co) it's a precondition that
--                                   cr = coercionRole co
downgradeRole_maybe Representational Nominal co = Just (mkSubCo co)
downgradeRole_maybe Nominal Representational _  = Nothing
downgradeRole_maybe Phantom Phantom          co = Just co
downgradeRole_maybe Phantom _                co = Just (toPhantomCo co)
downgradeRole_maybe _ Phantom                _  = Nothing
downgradeRole_maybe _ _                      co = Just co

-- | Like 'downgradeRole_maybe', but panics if the change isn't a downgrade.
-- See Note [Role twiddling functions]
downgradeRole :: Role  -- desired role
              -> Role  -- current role
              -> Coercion -> Coercion
downgradeRole r1 r2 co
  = case downgradeRole_maybe r1 r2 co of
      Just co' -> co'
      Nothing  -> pprPanic "downgradeRole" (ppr co)

-- | If the EqRel is ReprEq, makes a SubCo; otherwise, does nothing.
-- Note that the input coercion should always be nominal.
maybeSubCo :: EqRel -> Coercion -> Coercion
maybeSubCo NomEq  = id
maybeSubCo ReprEq = mkSubCo


mkAxiomRuleCo :: CoAxiomRule -> [Coercion] -> Coercion
mkAxiomRuleCo = AxiomRuleCo

-- | Make a "coercion between coercions".
mkProofIrrelCo :: Role       -- ^ role of the created coercion, "r"
               -> Coercion   -- ^ :: phi1 ~N phi2
               -> Coercion   -- ^ g1 :: phi1
               -> Coercion   -- ^ g2 :: phi2
               -> Coercion   -- ^ :: g1 ~r g2

-- if the two coercion prove the same fact, I just don't care what
-- the individual coercions are.
mkProofIrrelCo r (Refl {}) g  _  = Refl r (CoercionTy g)
mkProofIrrelCo r kco       g1 g2 = mkUnivCo (ProofIrrelProv kco) r
                                     (mkCoercionTy g1) (mkCoercionTy g2)

{-
%************************************************************************
%*                                                                      *
   Roles
%*                                                                      *
%************************************************************************
-}

-- | Converts a coercion to be nominal, if possible.
-- See Note [Role twiddling functions]
setNominalRole_maybe :: Coercion -> Maybe Coercion
setNominalRole_maybe co
  | Nominal <- coercionRole co = Just co
setNominalRole_maybe (SubCo co)  = Just co
setNominalRole_maybe (Refl _ ty) = Just $ Refl Nominal ty
setNominalRole_maybe (TyConAppCo Representational tc cos)
  = do { cos' <- mapM setNominalRole_maybe cos
       ; return $ TyConAppCo Nominal tc cos' }
setNominalRole_maybe (FunCo Representational co1 co2)
  = do { co1' <- setNominalRole_maybe co1
       ; co2' <- setNominalRole_maybe co2
       ; return $ FunCo Nominal co1' co2'
       }
setNominalRole_maybe (SymCo co)
  = SymCo <$> setNominalRole_maybe co
setNominalRole_maybe (TransCo co1 co2)
  = TransCo <$> setNominalRole_maybe co1 <*> setNominalRole_maybe co2
setNominalRole_maybe (AppCo co1 co2)
  = AppCo <$> setNominalRole_maybe co1 <*> pure co2
setNominalRole_maybe (ForAllCo tv kind_co co)
  = ForAllCo tv kind_co <$> setNominalRole_maybe co
setNominalRole_maybe (NthCo n co)
  = NthCo n <$> setNominalRole_maybe co
setNominalRole_maybe (InstCo co arg)
  = InstCo <$> setNominalRole_maybe co <*> pure arg
setNominalRole_maybe (CoherenceCo co1 co2)
  = CoherenceCo <$> setNominalRole_maybe co1 <*> pure co2
setNominalRole_maybe (UnivCo prov _ co1 co2)
  | case prov of UnsafeCoerceProv -> True   -- it's always unsafe
                 PhantomProv _    -> False  -- should always be phantom
                 ProofIrrelProv _ -> True   -- it's always safe
                 PluginProv _     -> False  -- who knows? This choice is conservative.
                 HoleProv _       -> False  -- no no no.
  = Just $ UnivCo prov Nominal co1 co2
setNominalRole_maybe _ = Nothing

-- | Make a phantom coercion between two types. The coercion passed
-- in must be a nominal coercion between the kinds of the
-- types.
mkPhantomCo :: Coercion -> Type -> Type -> Coercion
mkPhantomCo h t1 t2
  = mkUnivCo (PhantomProv h) Phantom t1 t2

-- | Make a phantom coercion between two types of the same kind.
mkHomoPhantomCo :: Type -> Type -> Coercion
mkHomoPhantomCo t1 t2
  = ASSERT( k1 `eqType` typeKind t2 )
    mkPhantomCo (mkNomReflCo k1) t1 t2
  where
    k1 = typeKind t1

-- takes any coercion and turns it into a Phantom coercion
toPhantomCo :: Coercion -> Coercion
toPhantomCo co
  = mkPhantomCo (mkKindCo co) ty1 ty2
  where Pair ty1 ty2 = coercionKind co

-- Convert args to a TyConAppCo Nominal to the same TyConAppCo Representational
applyRoles :: TyCon -> [Coercion] -> [Coercion]
applyRoles tc cos
  = zipWith (\r -> downgradeRole r Nominal) (tyConRolesRepresentational tc) cos

-- the Role parameter is the Role of the TyConAppCo
-- defined here because this is intimiately concerned with the implementation
-- of TyConAppCo
tyConRolesX :: Role -> TyCon -> [Role]
tyConRolesX Representational tc = tyConRolesRepresentational tc
tyConRolesX role             _  = repeat role

tyConRolesRepresentational :: TyCon -> [Role]
tyConRolesRepresentational tc = tyConRoles tc ++ repeat Nominal

nthRole :: Role -> TyCon -> Int -> Role
nthRole Nominal _ _ = Nominal
nthRole Phantom _ _ = Phantom
nthRole Representational tc n
  = (tyConRolesRepresentational tc) `getNth` n

ltRole :: Role -> Role -> Bool
-- Is one role "less" than another?
--     Nominal < Representational < Phantom
ltRole Phantom          _       = False
ltRole Representational Phantom = True
ltRole Representational _       = False
ltRole Nominal          Nominal = False
ltRole Nominal          _       = True

-------------------------------

-- | like mkKindCo, but aggressively & recursively optimizes to avoid using
-- a KindCo constructor. The output role is nominal.
promoteCoercion :: Coercion -> Coercion

-- First cases handles anything that should yield refl.
promoteCoercion co = case co of

    _ | ki1 `eqType` ki2
      -> mkNomReflCo (typeKind ty1)
     -- no later branch should return refl
     --    The ASSERT( False )s throughout
     -- are these cases explicitly, but they should never fire.

    Refl _ ty -> ASSERT( False )
                 mkNomReflCo (typeKind ty)

    TyConAppCo _ tc args
      | Just co' <- instCoercions (mkNomReflCo (tyConKind tc)) args
      -> co'
      | otherwise
      -> mkKindCo co

    AppCo co1 arg
      | Just co' <- instCoercion (coercionKind (mkKindCo co1))
                                 (promoteCoercion co1) arg
      -> co'
      | otherwise
      -> mkKindCo co

    ForAllCo _ _ g
      -> promoteCoercion g

    FunCo _ _ _
      -> mkNomReflCo liftedTypeKind

    CoVarCo {}
      -> mkKindCo co

    AxiomInstCo {}
      -> mkKindCo co

    UnivCo UnsafeCoerceProv _ t1 t2
      -> mkUnsafeCo Nominal (typeKind t1) (typeKind t2)
    UnivCo (PhantomProv kco) _ _ _
      -> kco
    UnivCo (ProofIrrelProv kco) _ _ _
      -> kco
    UnivCo (PluginProv _) _ _ _
      -> mkKindCo co
    UnivCo (HoleProv _) _ _ _
      -> mkKindCo co

    SymCo g
      -> mkSymCo (promoteCoercion g)

    TransCo co1 co2
      -> mkTransCo (promoteCoercion co1) (promoteCoercion co2)

    NthCo n co1
      | Just (_, args) <- splitTyConAppCo_maybe co1
      , args `lengthExceeds` n
      -> promoteCoercion (args !! n)

      | Just _ <- splitForAllCo_maybe co
      , n == 0
      -> ASSERT( False ) mkNomReflCo liftedTypeKind

      | otherwise
      -> mkKindCo co

    LRCo lr co1
      | Just (lco, rco) <- splitAppCo_maybe co1
      -> case lr of
           CLeft  -> promoteCoercion lco
           CRight -> promoteCoercion rco

      | otherwise
      -> mkKindCo co

    InstCo g _
      -> promoteCoercion g

    CoherenceCo g h
      -> mkSymCo h `mkTransCo` promoteCoercion g

    KindCo _
      -> ASSERT( False )
         mkNomReflCo liftedTypeKind

    SubCo g
      -> promoteCoercion g

    AxiomRuleCo {}
      -> mkKindCo co

  where
    Pair ty1 ty2 = coercionKind co
    ki1 = typeKind ty1
    ki2 = typeKind ty2

-- | say @g = promoteCoercion h@. Then, @instCoercion g w@ yields @Just g'@,
-- where @g' = promoteCoercion (h w)@.
-- fails if this is not possible, if @g@ coerces between a forall and an ->
-- or if second parameter has a representational role and can't be used
-- with an InstCo. The result role matches is representational.
instCoercion :: Pair Type -- type of the first coercion
             -> Coercion  -- ^ must be nominal
             -> Coercion
             -> Maybe Coercion
instCoercion (Pair lty rty) g w
  | isForAllTy lty && isForAllTy rty
  , Just w' <- setNominalRole_maybe w
  = Just $ mkInstCo g w'
  | isFunTy lty && isFunTy rty
  = Just $ mkNthCo 3 g -- extract result type, which is the 4th argument to (->)
  | otherwise -- one forall, one funty...
  = Nothing
  where

instCoercions :: Coercion -> [Coercion] -> Maybe Coercion
instCoercions g ws
  = let arg_ty_pairs = map coercionKind ws in
    snd <$> foldM go (coercionKind g, g) (zip arg_ty_pairs ws)
  where
    go :: (Pair Type, Coercion) -> (Pair Type, Coercion)
       -> Maybe (Pair Type, Coercion)
    go (g_tys, g) (w_tys, w)
      = do { g' <- instCoercion g_tys g w
           ; return (piResultTy <$> g_tys <*> w_tys, g') }

-- | Creates a new coercion with both of its types casted by different casts
-- castCoercionKind g h1 h2, where g :: t1 ~ t2, has type (t1 |> h1) ~ (t2 |> h2)
-- The second and third coercions must be nominal.
castCoercionKind :: Coercion -> Coercion -> Coercion -> Coercion
castCoercionKind g h1 h2
  = g `mkCoherenceLeftCo` h1 `mkCoherenceRightCo` h2

-- See note [Newtype coercions] in TyCon

mkPiCos :: Role -> [Var] -> Coercion -> Coercion
mkPiCos r vs co = foldr (mkPiCo r) co vs

-- | Make a forall 'Coercion', where both types related by the coercion
-- are quantified over the same type variable.
mkPiCo  :: Role -> Var -> Coercion -> Coercion
mkPiCo r v co | isTyVar v = mkHomoForAllCos [v] co
              | otherwise = mkFunCo r (mkReflCo r (varType v)) co

-- The second coercion is sometimes lifted (~) and sometimes unlifted (~#).
-- So, we have to make sure to supply the right parameter to decomposeCo.
-- mkCoCast (c :: s1 ~# t1) (g :: (s1 ~# s2) ~# (t1 ~# t2)) :: s2 ~# t2
-- Both coercions *must* have the same role.
mkCoCast :: Coercion -> Coercion -> Coercion
mkCoCast c g
  = mkSymCo g1 `mkTransCo` c `mkTransCo` g2
  where
       -- g  :: (s1 ~# s2) ~# (t1 ~#  t2)
       -- g1 :: s1 ~# t1
       -- g2 :: s2 ~# t2
    (_, args) = splitTyConApp (pFst $ coercionKind g)
    n_args = length args
    co_list = decomposeCo n_args g
    g1 = co_list `getNth` (n_args - 2)
    g2 = co_list `getNth` (n_args - 1)

{-
%************************************************************************
%*                                                                      *
            Newtypes
%*                                                                      *
%************************************************************************
-}

-- | If @co :: T ts ~ rep_ty@ then:
--
-- > instNewTyCon_maybe T ts = Just (rep_ty, co)
--
-- Checks for a newtype, and for being saturated
instNewTyCon_maybe :: TyCon -> [Type] -> Maybe (Type, Coercion)
instNewTyCon_maybe tc tys
  | Just (tvs, ty, co_tc) <- unwrapNewTyConEtad_maybe tc  -- Check for newtype
  , tvs `leLength` tys                                    -- Check saturated enough
  = Just (applyTysX tvs ty tys, mkUnbranchedAxInstCo Representational co_tc tys [])
  | otherwise
  = Nothing

{-
************************************************************************
*                                                                      *
         Type normalisation
*                                                                      *
************************************************************************
-}

-- | A function to check if we can reduce a type by one step. Used
-- with 'topNormaliseTypeX'.
type NormaliseStepper ev = RecTcChecker
                         -> TyCon     -- tc
                         -> [Type]    -- tys
                         -> NormaliseStepResult ev

-- | The result of stepping in a normalisation function.
-- See 'topNormaliseTypeX'.
data NormaliseStepResult ev
  = NS_Done   -- ^ Nothing more to do
  | NS_Abort  -- ^ Utter failure. The outer function should fail too.
  | NS_Step RecTcChecker Type ev    -- ^ We stepped, yielding new bits;
                                    -- ^ ev is evidence;
                                    -- Usually a co :: old type ~ new type

mapStepResult :: (ev1 -> ev2)
              -> NormaliseStepResult ev1 -> NormaliseStepResult ev2
mapStepResult f (NS_Step rec_nts ty ev) = NS_Step rec_nts ty (f ev)
mapStepResult _ NS_Done                 = NS_Done
mapStepResult _ NS_Abort                = NS_Abort

-- | Try one stepper and then try the next, if the first doesn't make
-- progress.
-- So if it returns NS_Done, it means that both steppers are satisfied
composeSteppers :: NormaliseStepper ev -> NormaliseStepper ev
                -> NormaliseStepper ev
composeSteppers step1 step2 rec_nts tc tys
  = case step1 rec_nts tc tys of
      success@(NS_Step {}) -> success
      NS_Done              -> step2 rec_nts tc tys
      NS_Abort             -> NS_Abort

-- | A 'NormaliseStepper' that unwraps newtypes, careful not to fall into
-- a loop. If it would fall into a loop, it produces 'NS_Abort'.
unwrapNewTypeStepper :: NormaliseStepper Coercion
unwrapNewTypeStepper rec_nts tc tys
  | Just (ty', co) <- instNewTyCon_maybe tc tys
  = case checkRecTc rec_nts tc of
      Just rec_nts' -> NS_Step rec_nts' ty' co
      Nothing       -> NS_Abort

  | otherwise
  = NS_Done

-- | A general function for normalising the top-level of a type. It continues
-- to use the provided 'NormaliseStepper' until that function fails, and then
-- this function returns. The roles of the coercions produced by the
-- 'NormaliseStepper' must all be the same, which is the role returned from
-- the call to 'topNormaliseTypeX'.
--
-- Typically ev is Coercion.
--
-- If topNormaliseTypeX step plus ty = Just (ev, ty')
-- then ty ~ev1~ t1 ~ev2~ t2 ... ~evn~ ty'
-- and ev = ev1 `plus` ev2 `plus` ... `plus` evn
-- If it returns Nothing then no newtype unwrapping could happen
topNormaliseTypeX :: NormaliseStepper ev -> (ev -> ev -> ev)
                  -> Type -> Maybe (ev, Type)
topNormaliseTypeX stepper plus ty
 | Just (tc, tys) <- splitTyConApp_maybe ty
 , NS_Step rec_nts ty' ev <- stepper initRecTc tc tys
 = go rec_nts ev ty'
 | otherwise
 = Nothing
 where
    go rec_nts ev ty
      | Just (tc, tys) <- splitTyConApp_maybe ty
      = case stepper rec_nts tc tys of
          NS_Step rec_nts' ty' ev' -> go rec_nts' (ev `plus` ev') ty'
          NS_Done  -> Just (ev, ty)
          NS_Abort -> Nothing

      | otherwise
      = Just (ev, ty)

topNormaliseNewType_maybe :: Type -> Maybe (Coercion, Type)
-- ^ Sometimes we want to look through a @newtype@ and get its associated coercion.
-- This function strips off @newtype@ layers enough to reveal something that isn't
-- a @newtype@.  Specifically, here's the invariant:
--
-- > topNormaliseNewType_maybe rec_nts ty = Just (co, ty')
--
-- then (a)  @co : ty0 ~ ty'@.
--      (b)  ty' is not a newtype.
--
-- The function returns @Nothing@ for non-@newtypes@,
-- or unsaturated applications
--
-- This function does *not* look through type families, because it has no access to
-- the type family environment. If you do have that at hand, consider to use
-- topNormaliseType_maybe, which should be a drop-in replacement for
-- topNormaliseNewType_maybe
-- If topNormliseNewType_maybe ty = Just (co, ty'), then co : ty ~R ty'
topNormaliseNewType_maybe ty
  = topNormaliseTypeX unwrapNewTypeStepper mkTransCo ty

{-
%************************************************************************
%*                                                                      *
                   Comparison of coercions
%*                                                                      *
%************************************************************************
-}

-- | Syntactic equality of coercions
eqCoercion :: Coercion -> Coercion -> Bool
eqCoercion = eqType `on` coercionType

-- | Compare two 'Coercion's, with respect to an RnEnv2
eqCoercionX :: RnEnv2 -> Coercion -> Coercion -> Bool
eqCoercionX env = eqTypeX env `on` coercionType

{-
%************************************************************************
%*                                                                      *
                   "Lifting" substitution
           [(TyCoVar,Coercion)] -> Type -> Coercion
%*                                                                      *
%************************************************************************

Note [Lifting coercions over types: liftCoSubst]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The KPUSH rule deals with this situation
   data T a = MkK (a -> Maybe a)
   g :: T t1 ~ K t2
   x :: t1 -> Maybe t1

   case (K @t1 x) |> g of
     K (y:t2 -> Maybe t2) -> rhs

We want to push the coercion inside the constructor application.
So we do this

   g' :: t1~t2  =  Nth 0 g

   case K @t2 (x |> g' -> Maybe g') of
     K (y:t2 -> Maybe t2) -> rhs

The crucial operation is that we
  * take the type of K's argument: a -> Maybe a
  * and substitute g' for a
thus giving *coercion*.  This is what liftCoSubst does.

In the presence of kind coercions, this is a bit
of a hairy operation. So, we refer you to the paper introducing kind coercions,
available at www.cis.upenn.edu/~sweirich/papers/fckinds-extended.pdf
-}

-- ----------------------------------------------------
-- See Note [Lifting coercions over types: liftCoSubst]
-- ----------------------------------------------------

data LiftingContext = LC TCvSubst LiftCoEnv
  -- in optCoercion, we need to lift when optimizing InstCo.
  -- See Note [Optimising InstCo] in OptCoercion
  -- We thus propagate the substitution from OptCoercion here.

instance Outputable LiftingContext where
  ppr (LC _ env) = hang (text "LiftingContext:") 2 (ppr env)

type LiftCoEnv = VarEnv Coercion
     -- Maps *type variables* to *coercions*.
     -- That's the whole point of this function!

-- like liftCoSubstWith, but allows for existentially-bound types as well
liftCoSubstWithEx :: Role          -- desired role for output coercion
                  -> [TyVar]       -- universally quantified tyvars
                  -> [Coercion]    -- coercions to substitute for those
                  -> [TyVar]       -- existentially quantified tyvars
                  -> [Type]        -- types to be bound to ex vars
                  -> (Type -> Coercion, [Type]) -- (lifting function, converted ex args)
liftCoSubstWithEx role univs omegas exs rhos
  = let theta = mkLiftingContext (zipEqual "liftCoSubstWithExU" univs omegas)
        psi   = extendLiftingContextEx theta (zipEqual "liftCoSubstWithExX" exs rhos)
    in (ty_co_subst psi role, substTyVars (lcSubstRight psi) exs)

liftCoSubstWith :: Role -> [TyCoVar] -> [Coercion] -> Type -> Coercion
-- NB: This really can be called with CoVars, when optimising axioms.
liftCoSubstWith r tvs cos ty
  = liftCoSubst r (mkLiftingContext $ zipEqual "liftCoSubstWith" tvs cos) ty

-- | @liftCoSubst role lc ty@ produces a coercion (at role @role@)
-- that coerces between @lc_left(ty)@ and @lc_right(ty)@, where
-- @lc_left@ is a substitution mapping type variables to the left-hand
-- types of the mapped coercions in @lc@, and similar for @lc_right@.
liftCoSubst :: HasDebugCallStack => Role -> LiftingContext -> Type -> Coercion
liftCoSubst r lc@(LC subst env) ty
  | isEmptyVarEnv env = Refl r (substTy subst ty)
  | otherwise         = ty_co_subst lc r ty

emptyLiftingContext :: InScopeSet -> LiftingContext
emptyLiftingContext in_scope = LC (mkEmptyTCvSubst in_scope) emptyVarEnv

mkLiftingContext :: [(TyCoVar,Coercion)] -> LiftingContext
mkLiftingContext pairs
  = LC (mkEmptyTCvSubst $ mkInScopeSet $ tyCoVarsOfCos (map snd pairs))
       (mkVarEnv pairs)

mkSubstLiftingContext :: TCvSubst -> LiftingContext
mkSubstLiftingContext subst = LC subst emptyVarEnv

-- | Extend a lifting context with a new /type/ mapping.
extendLiftingContext :: LiftingContext  -- ^ original LC
                     -> TyVar           -- ^ new variable to map...
                     -> Coercion        -- ^ ...to this lifted version
                     -> LiftingContext
extendLiftingContext (LC subst env) tv arg
  = ASSERT( isTyVar tv )
    LC subst (extendVarEnv env tv arg)

-- | Extend a lifting context with existential-variable bindings.
-- This follows the lifting context extension definition in the
-- "FC with Explicit Kind Equality" paper.
extendLiftingContextEx :: LiftingContext    -- ^ original lifting context
                       -> [(TyVar,Type)]    -- ^ ex. var / value pairs
                       -> LiftingContext
-- Note that this is more involved than extendLiftingContext. That function
-- takes a coercion to extend with, so it's assumed that the caller has taken
-- into account any of the kind-changing stuff worried about here.
extendLiftingContextEx lc [] = lc
extendLiftingContextEx lc@(LC subst env) ((v,ty):rest)
-- This function adds bindings for *Nominal* coercions. Why? Because it
-- works with existentially bound variables, which are considered to have
-- nominal roles.
  = let lc' = LC (subst `extendTCvInScopeSet` tyCoVarsOfType ty)
                 (extendVarEnv env v (mkSymCo $ mkCoherenceCo
                                         (mkNomReflCo ty)
                                         (ty_co_subst lc Nominal (tyVarKind v))))
    in extendLiftingContextEx lc' rest

-- | Erase the environments in a lifting context
zapLiftingContext :: LiftingContext -> LiftingContext
zapLiftingContext (LC subst _) = LC (zapTCvSubst subst) emptyVarEnv

-- | Like 'substForAllCoBndr', but works on a lifting context
substForAllCoBndrCallbackLC :: Bool
                            -> (Coercion -> Coercion)
                            -> LiftingContext -> TyVar -> Coercion
                            -> (LiftingContext, TyVar, Coercion)
substForAllCoBndrCallbackLC sym sco (LC subst lc_env) tv co
  = (LC subst' lc_env, tv', co')
  where
    (subst', tv', co') = substForAllCoBndrCallback sym sco subst tv co

-- | The \"lifting\" operation which substitutes coercions for type
--   variables in a type to produce a coercion.
--
--   For the inverse operation, see 'liftCoMatch'
ty_co_subst :: LiftingContext -> Role -> Type -> Coercion
ty_co_subst lc role ty
  = go role ty
  where
    go :: Role -> Type -> Coercion
    go Phantom ty          = lift_phantom ty
    go r (TyVarTy tv)      = expectJust "ty_co_subst bad roles" $
                             liftCoSubstTyVar lc r tv
    go r (AppTy ty1 ty2)   = mkAppCo (go r ty1) (go Nominal ty2)
    go r (TyConApp tc tys) = mkTyConAppCo r tc (zipWith go (tyConRolesX r tc) tys)
    go r (FunTy ty1 ty2)   = mkFunCo r (go r ty1) (go r ty2)
    go r (ForAllTy (TvBndr v _) ty)
                           = let (lc', v', h) = liftCoSubstVarBndr lc v in
                             mkForAllCo v' h $! ty_co_subst lc' r ty
    go r ty@(LitTy {})     = ASSERT( r == Nominal )
                             mkReflCo r ty
    go r (CastTy ty co)    = castCoercionKind (go r ty) (substLeftCo lc co)
                                                        (substRightCo lc co)
    go r (CoercionTy co)   = mkProofIrrelCo r kco (substLeftCo lc co)
                                                  (substRightCo lc co)
      where kco = go Nominal (coercionType co)

    lift_phantom ty = mkPhantomCo (go Nominal (typeKind ty))
                                  (substTy (lcSubstLeft  lc) ty)
                                  (substTy (lcSubstRight lc) ty)

{-
Note [liftCoSubstTyVar]
~~~~~~~~~~~~~~~~~~~~~~~~~
This function can fail if a coercion in the environment is of too low a role.

liftCoSubstTyVar is called from two places: in liftCoSubst (naturally), and
also in matchAxiom in OptCoercion. From liftCoSubst, the so-called lifting
lemma guarantees that the roles work out. If we fail in this
case, we really should panic -- something is deeply wrong. But, in matchAxiom,
failing is fine. matchAxiom is trying to find a set of coercions
that match, but it may fail, and this is healthy behavior.
-}

-- See Note [liftCoSubstTyVar]
liftCoSubstTyVar :: LiftingContext -> Role -> TyVar -> Maybe Coercion
liftCoSubstTyVar (LC subst env) r v
  | Just co_arg <- lookupVarEnv env v
  = downgradeRole_maybe r (coercionRole co_arg) co_arg

  | otherwise
  = Just $ Refl r (substTyVar subst v)

liftCoSubstVarBndr :: LiftingContext -> TyVar
                   -> (LiftingContext, TyVar, Coercion)
liftCoSubstVarBndr lc tv
  = let (lc', tv', h, _) = liftCoSubstVarBndrCallback callback lc tv in
    (lc', tv', h)
  where
    callback lc' ty' = (ty_co_subst lc' Nominal ty', ())

-- the callback must produce a nominal coercion
liftCoSubstVarBndrCallback :: (LiftingContext -> Type -> (Coercion, a))
                           -> LiftingContext -> TyVar
                           -> (LiftingContext, TyVar, Coercion, a)
liftCoSubstVarBndrCallback fun lc@(LC subst cenv) old_var
  = ( LC (subst `extendTCvInScope` new_var) new_cenv
    , new_var, eta, stuff )
  where
    old_kind     = tyVarKind old_var
    (eta, stuff) = fun lc old_kind
    Pair k1 _    = coercionKind eta
    new_var      = uniqAway (getTCvInScope subst) (setVarType old_var k1)

    lifted   = Refl Nominal (TyVarTy new_var)
    new_cenv = extendVarEnv cenv old_var lifted

-- | Is a var in the domain of a lifting context?
isMappedByLC :: TyCoVar -> LiftingContext -> Bool
isMappedByLC tv (LC _ env) = tv `elemVarEnv` env

-- If [a |-> g] is in the substitution and g :: t1 ~ t2, substitute a for t1
-- If [a |-> (g1, g2)] is in the substitution, substitute a for g1
substLeftCo :: LiftingContext -> Coercion -> Coercion
substLeftCo lc co
  = substCo (lcSubstLeft lc) co

-- Ditto, but for t2 and g2
substRightCo :: LiftingContext -> Coercion -> Coercion
substRightCo lc co
  = substCo (lcSubstRight lc) co

-- | Apply "sym" to all coercions in a 'LiftCoEnv'
swapLiftCoEnv :: LiftCoEnv -> LiftCoEnv
swapLiftCoEnv = mapVarEnv mkSymCo

lcSubstLeft :: LiftingContext -> TCvSubst
lcSubstLeft (LC subst lc_env) = liftEnvSubstLeft subst lc_env

lcSubstRight :: LiftingContext -> TCvSubst
lcSubstRight (LC subst lc_env) = liftEnvSubstRight subst lc_env

liftEnvSubstLeft :: TCvSubst -> LiftCoEnv -> TCvSubst
liftEnvSubstLeft = liftEnvSubst pFst

liftEnvSubstRight :: TCvSubst -> LiftCoEnv -> TCvSubst
liftEnvSubstRight = liftEnvSubst pSnd

liftEnvSubst :: (forall a. Pair a -> a) -> TCvSubst -> LiftCoEnv -> TCvSubst
liftEnvSubst selector subst lc_env
  = composeTCvSubst (TCvSubst emptyInScopeSet tenv cenv) subst
  where
    pairs            = nonDetUFMToList lc_env
                       -- It's OK to use nonDetUFMToList here because we
                       -- immediately forget the ordering by creating
                       -- a VarEnv
    (tpairs, cpairs) = partitionWith ty_or_co pairs
    tenv             = mkVarEnv_Directly tpairs
    cenv             = mkVarEnv_Directly cpairs

    ty_or_co :: (Unique, Coercion) -> Either (Unique, Type) (Unique, Coercion)
    ty_or_co (u, co)
      | Just equality_co <- isCoercionTy_maybe equality_ty
      = Right (u, equality_co)
      | otherwise
      = Left (u, equality_ty)
      where
        equality_ty = selector (coercionKind co)

-- | Extract the underlying substitution from the LiftingContext
lcTCvSubst :: LiftingContext -> TCvSubst
lcTCvSubst (LC subst _) = subst

-- | Get the 'InScopeSet' from a 'LiftingContext'
lcInScopeSet :: LiftingContext -> InScopeSet
lcInScopeSet (LC subst _) = getTCvInScope subst

{-
%************************************************************************
%*                                                                      *
            Sequencing on coercions
%*                                                                      *
%************************************************************************
-}

seqCo :: Coercion -> ()
seqCo (Refl r ty)               = r `seq` seqType ty
seqCo (TyConAppCo r tc cos)     = r `seq` tc `seq` seqCos cos
seqCo (AppCo co1 co2)           = seqCo co1 `seq` seqCo co2
seqCo (ForAllCo tv k co)        = seqType (tyVarKind tv) `seq` seqCo k
                                                         `seq` seqCo co
seqCo (FunCo r co1 co2)         = r `seq` seqCo co1 `seq` seqCo co2
seqCo (CoVarCo cv)              = cv `seq` ()
seqCo (AxiomInstCo con ind cos) = con `seq` ind `seq` seqCos cos
seqCo (UnivCo p r t1 t2)
  = seqProv p `seq` r `seq` seqType t1 `seq` seqType t2
seqCo (SymCo co)                = seqCo co
seqCo (TransCo co1 co2)         = seqCo co1 `seq` seqCo co2
seqCo (NthCo n co)              = n `seq` seqCo co
seqCo (LRCo lr co)              = lr `seq` seqCo co
seqCo (InstCo co arg)           = seqCo co `seq` seqCo arg
seqCo (CoherenceCo co1 co2)     = seqCo co1 `seq` seqCo co2
seqCo (KindCo co)               = seqCo co
seqCo (SubCo co)                = seqCo co
seqCo (AxiomRuleCo _ cs)        = seqCos cs

seqProv :: UnivCoProvenance -> ()
seqProv UnsafeCoerceProv    = ()
seqProv (PhantomProv co)    = seqCo co
seqProv (ProofIrrelProv co) = seqCo co
seqProv (PluginProv _)      = ()
seqProv (HoleProv _)        = ()

seqCos :: [Coercion] -> ()
seqCos []       = ()
seqCos (co:cos) = seqCo co `seq` seqCos cos

{-
%************************************************************************
%*                                                                      *
             The kind of a type, and of a coercion
%*                                                                      *
%************************************************************************

Note [Computing a coercion kind and role]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To compute a coercion's kind is straightforward: see coercionKind.
But to compute a coercion's role, in the case for NthCo we need
its kind as well.  So if we have two separate functions (one for kinds
and one for roles) we can get exponentially bad behaviour, since each
NthCo node makes a separate call to coercionKind, which traverses the
sub-tree again.  This was part of the problem in Trac #9233.

Solution: compute both together; hence coercionKindRole.  We keep a
separate coercionKind function because it's a bit more efficient if
the kind is all you want.
-}

coercionType :: Coercion -> Type
coercionType co = case coercionKindRole co of
  (Pair ty1 ty2, r) -> mkCoercionType r ty1 ty2

------------------
-- | If it is the case that
--
-- > c :: (t1 ~ t2)
--
-- i.e. the kind of @c@ relates @t1@ and @t2@, then @coercionKind c = Pair t1 t2@.

coercionKind :: Coercion -> Pair Type
coercionKind co = go co
  where
    go (Refl _ ty)          = Pair ty ty
    go (TyConAppCo _ tc cos)= mkTyConApp tc <$> (sequenceA $ map go cos)
    go (AppCo co1 co2)      = mkAppTy <$> go co1 <*> go co2
    go (ForAllCo tv1 k_co co)
      = let Pair _ k2          = go k_co
            tv2                = setTyVarKind tv1 k2
            Pair ty1 ty2       = go co
            subst = zipTvSubst [tv1] [TyVarTy tv2 `mk_cast_ty` mkSymCo k_co]
            ty2' = substTyAddInScope subst ty2 in
            -- We need free vars of ty2 in scope to satisfy the invariant
            -- from Note [The substitution invariant]
            -- This is doing repeated substitutions and probably doesn't
            -- need to, see #11735
        mkInvForAllTy <$> Pair tv1 tv2 <*> Pair ty1 ty2'
    go (FunCo _ co1 co2)    = mkFunTy <$> go co1 <*> go co2
    go (CoVarCo cv)         = coVarTypes cv
    go (AxiomInstCo ax ind cos)
      | CoAxBranch { cab_tvs = tvs, cab_cvs = cvs
                   , cab_lhs = lhs, cab_rhs = rhs } <- coAxiomNthBranch ax ind
      , let Pair tycos1 tycos2 = sequenceA (map go cos)
            (tys1, cotys1) = splitAtList tvs tycos1
            (tys2, cotys2) = splitAtList tvs tycos2
            cos1           = map stripCoercionTy cotys1
            cos2           = map stripCoercionTy cotys2
      = ASSERT( cos `equalLength` (tvs ++ cvs) )
                  -- Invariant of AxiomInstCo: cos should
                  -- exactly saturate the axiom branch
        Pair (substTyWith tvs tys1 $
              substTyWithCoVars cvs cos1 $
              mkTyConApp (coAxiomTyCon ax) lhs)
             (substTyWith tvs tys2 $
              substTyWithCoVars cvs cos2 rhs)
    go (UnivCo _ _ ty1 ty2)   = Pair ty1 ty2
    go (SymCo co)             = swap $ go co
    go (TransCo co1 co2)      = Pair (pFst $ go co1) (pSnd $ go co2)
    go g@(NthCo d co)
      | Just argss <- traverse tyConAppArgs_maybe tys
      = ASSERT( and $ (`lengthExceeds` d) <$> argss )
        (`getNth` d) <$> argss

      | d == 0
      , Just splits <- traverse splitForAllTy_maybe tys
      = (tyVarKind . fst) <$> splits

      | otherwise
      = pprPanic "coercionKind" (ppr g)
      where
        tys = go co
    go (LRCo lr co)         = (pickLR lr . splitAppTy) <$> go co
    go (InstCo aco arg)     = go_app aco [arg]
    go (CoherenceCo g h)
      = let Pair ty1 ty2 = go g in
        Pair (mkCastTy ty1 h) ty2
    go (KindCo co)          = typeKind <$> go co
    go (SubCo co)           = go co
    go (AxiomRuleCo ax cos) = expectJust "coercionKind" $
                              coaxrProves ax (map go cos)

    go_app :: Coercion -> [Coercion] -> Pair Type
    -- Collect up all the arguments and apply all at once
    -- See Note [Nested InstCos]
    go_app (InstCo co arg) args = go_app co (arg:args)
    go_app co              args = piResultTys <$> go co <*> (sequenceA $ map go args)

    -- The real mkCastTy is too slow, and we can easily have nested ForAllCos.
    mk_cast_ty :: Type -> Coercion -> Type
    mk_cast_ty ty (Refl {}) = ty
    mk_cast_ty ty co        = CastTy ty co

-- | Apply 'coercionKind' to multiple 'Coercion's
coercionKinds :: [Coercion] -> Pair [Type]
coercionKinds tys = sequenceA $ map coercionKind tys

-- | Get a coercion's kind and role.
-- Why both at once?  See Note [Computing a coercion kind and role]
coercionKindRole :: Coercion -> (Pair Type, Role)
coercionKindRole = go
  where
    go (Refl r ty) = (Pair ty ty, r)
    go (TyConAppCo r tc cos)
      = (mkTyConApp tc <$> (sequenceA $ map coercionKind cos), r)
    go (AppCo co1 co2)
      = let (tys1, r1) = go co1 in
        (mkAppTy <$> tys1 <*> coercionKind co2, r1)
    go (ForAllCo tv1 k_co co)
      = let Pair _ k2          = coercionKind k_co
            tv2                = setTyVarKind tv1 k2
            (Pair ty1 ty2, r)  = go co
            subst = zipTvSubst [tv1] [TyVarTy tv2 `mkCastTy` mkSymCo k_co]
            ty2' = substTyAddInScope subst ty2 in
            -- We need free vars of ty2 in scope to satisfy the invariant
            -- from Note [The substitution invariant]
            -- This is doing repeated substitutions and probably doesn't
            -- need to, see #11735
        (mkInvForAllTy <$> Pair tv1 tv2 <*> Pair ty1 ty2', r)
    go (FunCo r co1 co2)
      = (mkFunTy <$> coercionKind co1 <*> coercionKind co2, r)
    go (CoVarCo cv) = (coVarTypes cv, coVarRole cv)
    go co@(AxiomInstCo ax _ _) = (coercionKind co, coAxiomRole ax)
    go (UnivCo _ r ty1 ty2)  = (Pair ty1 ty2, r)
    go (SymCo co) = first swap $ go co
    go (TransCo co1 co2)
      = let (tys1, r) = go co1 in
        (Pair (pFst tys1) (pSnd $ coercionKind co2), r)
    go (NthCo d co)
      | Just (tv1, _) <- splitForAllTy_maybe ty1
      = ASSERT( d == 0 )
        let (tv2, _) = splitForAllTy ty2 in
        (tyVarKind <$> Pair tv1 tv2, Nominal)

      | otherwise
      = let (tc1,  args1) = splitTyConApp ty1
            (_tc2, args2) = splitTyConApp ty2
        in
        ASSERT2( tc1 == _tc2, ppr d $$ ppr tc1 $$ ppr _tc2 )
        ((`getNth` d) <$> Pair args1 args2, nthRole r tc1 d)

      where
        (Pair ty1 ty2, r) = go co
    go co@(LRCo {}) = (coercionKind co, Nominal)
    go (InstCo co arg) = go_app co [arg]
    go (CoherenceCo co1 co2)
      = let (Pair t1 t2, r) = go co1 in
        (Pair (t1 `mkCastTy` co2) t2, r)
    go co@(KindCo {}) = (coercionKind co, Nominal)
    go (SubCo co) = (coercionKind co, Representational)
    go co@(AxiomRuleCo ax _) = (coercionKind co, coaxrRole ax)

    go_app :: Coercion -> [Coercion] -> (Pair Type, Role)
    -- Collect up all the arguments and apply all at once
    -- See Note [Nested InstCos]
    go_app (InstCo co arg) args = go_app co (arg:args)
    go_app co              args
      = let (pair, r) = go co in
        (piResultTys <$> pair <*> (sequenceA $ map coercionKind args), r)

-- | Retrieve the role from a coercion.
coercionRole :: Coercion -> Role
coercionRole = snd . coercionKindRole
  -- There's not a better way to do this, because NthCo needs the *kind*
  -- and role of its argument. Luckily, laziness should generally avoid
  -- the need for computing kinds in other cases.

{-
Note [Nested InstCos]
~~~~~~~~~~~~~~~~~~~~~
In Trac #5631 we found that 70% of the entire compilation time was
being spent in coercionKind!  The reason was that we had
   (g @ ty1 @ ty2 .. @ ty100)    -- The "@s" are InstCos
where
   g :: forall a1 a2 .. a100. phi
If we deal with the InstCos one at a time, we'll do this:
   1.  Find the kind of (g @ ty1 .. @ ty99) : forall a100. phi'
   2.  Substitute phi'[ ty100/a100 ], a single tyvar->type subst
But this is a *quadratic* algorithm, and the blew up Trac #5631.
So it's very important to do the substitution simultaneously;
cf Type.piResultTys (which in fact we call here).

-}
