{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module GHC.Core.Reduction
  (
     -- * Reductions
     Reduction(..), ReductionN, ReductionR, HetReduction(..),
     Reductions(..),
     mkReduction, mkReductions, mkHetReduction, mkDehydrateCoercionRedn,
     mkHydrateReductionDCoercion,
     mkSubRedn,
     mkTransRedn, mkCoherenceRightRedn, mkCoherenceRightMRedn,
     mkCastRedn1, mkCastRedn2,
     mkReflRedn, mkGReflRightRedn, mkGReflRightMRedn,
     mkGReflLeftRedn, mkGReflLeftMRedn,
     mkAppRedn, mkAppRedns, mkFunRedn,
     mkForAllRedn, mkHomoForAllRedn, mkTyConAppRedn, mkClassPredRedn,
     mkTyConAppRedn_MightBeSynonym,
     mkProofIrrelRedn, mkReflCoRedn,
     homogeniseHetRedn, homogeniseRedn,
     unzipRedns,
     mkReflRedns,
     mkReflDCos,

     -- * Rewriting type arguments
     ArgsReductions(..),
     simplifyArgsWorker

  ) where

import GHC.Prelude

import GHC.Core.Class      ( Class(..) )
import GHC.Core.Coercion
import GHC.Core.Predicate  ( mkClassPred )
import GHC.Core.TyCon      ( TyCon, expandSynTyCon_maybe )
import GHC.Core.Type

import GHC.Data.Pair       ( Pair(Pair) )

import GHC.Types.Var       ( setTyVarKind )
import GHC.Types.Var.Env   ( mkInScopeSet )
import GHC.Types.Var.Set   ( TyCoVarSet )

import GHC.Utils.Misc      ( HasDebugCallStack, equalLength )
import GHC.Utils.Outputable
import GHC.Utils.Panic     ( assertPpr, panic )

import Data.List           ( zipWith4 )

{-
%************************************************************************
%*                                                                      *
      Reductions
%*                                                                      *
%************************************************************************

Note [The Reduction type]
~~~~~~~~~~~~~~~~~~~~~~~~~
Many functions in the type-checker rewrite a type, using Given type equalities
or type-family reductions, and return a Reduction:

  data Reduction = Reduction Type Coercion !Type

When we rewrite ty at role r, producing Reduction ty' dco xi, we guarantee:

  RW1: ty' is equal to ty (up to zonking)
  RW2: followDCo r ty' dco is equal to xi (up to zonking)

In particular, this means that `dco :: ty' ~r xi`. Note that we need to use ty',
and not ty, to satisfy RW2; see Note [The Hydration invariant] in GHC.Core.Coercion.
It could be the case that `followDCo r ty dco` crashes, e.g. if `ty` is a metavariable
and `dco = TyConAppDCo ..`. This is why we store the LHS type in the Reduction too.

The order of the arguments to the constructor serves as a reminder
of what the Type is.  In

    Reduction ty' dco xi

the original type ty appears to the left, and the result appears on the right,
reminding us that we must have:

  dco :: ty' ~r xi

Example functions that use this datatype:

   GHC.Core.FamInstEnv.topNormaliseType_maybe
     :: FamInstEnvs -> Type -> Maybe Reduction
   GHC.Tc.Solver.Rewrite.rewrite
     :: CtEvidence -> TcType -> TcS Reduction

Having Reduction as a data type, with a strict Type field, rather than using
a tuple (with all fields lazy), gives several advantages (see #20161)
* The strictness in Type improved performance in rewriting of type families
  (around 2.5% improvement in T9872),
* Compared to the situation before, it gives improved consistency around
  orientation of rewritings, as a Reduction is always left-to-right
  (the coercion's RHS type is always the type stored in the 'Reduction').
  No more 'mkSymCo's needed to convert between left-to-right and right-to-left.
-}

-- | A 'Reduction' is the result of an operation that rewrites a type @ty_in@.
-- The 'Reduction' includes:
--
--  - an input type @ty_in'@, equal to @ty_in@ up to zonking,
--  - a directed coercion @dco@,
--  - the rewritten type @ty_out@
--
-- such that @dco :: ty_in' ~ ty_out@, where the role @r@ of the coercion is determined
-- by the context.
--
-- Invariant: it is always valid to call @followDCo r ty_in' dco@, as per
-- Note [The Hydration invariant] in GHC.Core.Coercion.
--
-- See Note [The Reduction type].
data Reduction =
  Reduction
    { reductionOriginalType :: Type
    , reductionDCoercion    :: DCoercion
    , reductionReducedType  :: !Type
    }
-- N.B. the 'Coercion' field must be lazy: see for instance GHC.Tc.Solver.Rewrite.rewrite_tyvar2
-- which returns an error in the 'Coercion' field when dealing with a Derived constraint
-- (which is OK as this Coercion gets ignored later).
-- We might want to revisit the strictness once Deriveds are removed.

-- | A 'Reduction' in which the 'Coercion' has 'Nominal' role.
type ReductionN = Reduction

-- | A 'Reduction' in which the 'Coercion' has 'Representational' role.
type ReductionR = Reduction

-- | Create a 'Reduction' from a pair of a 'Coercion' and a 'Type.
--
-- Pre-condition: the RHS type of the coercion matches the provided type
-- (perhaps up to zonking).
--
-- Use 'coercionRedn' when you only have the coercion.
mkReduction :: Type -> DCoercion -> Type -> Reduction
mkReduction lty co rty = Reduction lty co rty
{-# INLINE mkReduction #-}

instance Outputable Reduction where
  ppr redn =
    braces $ vcat
      [ text " reductionReducedType:" <+> ppr (reductionReducedType redn)
      , text "   reductionDCoercion:" <+> ppr (reductionDCoercion redn)
      ]

-- | Turn a 'Coercion' into a 'Reduction'
-- by inspecting the LHS and RHS types of the coercion, and dehydrating.
--
-- Prefer using 'mkReduction' wherever possible to avoid doing these indirections.
mkDehydrateCoercionRedn :: Coercion -> Reduction
mkDehydrateCoercionRedn co =
  Reduction (coercionLKind co) (mkDehydrateCo co) (coercionRKind co)
{-# INLINE mkDehydrateCoercionRedn #-}

-- | Hydrate the 'DCoercion' stored inside a 'Reduction' into a full-fledged 'Coercion'.
mkHydrateReductionDCoercion :: HasDebugCallStack => Role -> Reduction -> Coercion
mkHydrateReductionDCoercion r (Reduction lty dco rty) = mkHydrateDCo r lty dco (Just rty)
  -- N.B.: we use the LHS type stored in the 'Reduction' to ensure
  -- we satisfy the Hydration invariant of Note [The Hydration invariant]
  -- in GHC.Core.Coercion.
{-# INLINE mkHydrateReductionDCoercion #-}

-- | Downgrade the role of the directed coercion stored in the 'Reduction',
-- from 'Nominal' to 'Representational'.
mkSubRedn :: Reduction -> Reduction
mkSubRedn redn@(Reduction lhs dco rhs)
  = redn { reductionDCoercion = mkSubDCo lhs dco rhs }
{-# INLINE mkSubRedn #-}

-- | Compose two reductions.
--
-- Assumes that forming a composite is valid, i.e. the RHS type of
-- the first directed coercion equals, up to zonking,
-- the LHS type of the second directed coercion.
--
-- Warning: this function is not guaranteed to preserve the Hydration invariant
-- as required by Note [The Reduction type]. You must manually ensure this
-- invariant.
mkTransRedn :: Reduction -> Reduction -> Reduction
mkTransRedn (Reduction ty1 dco1 _) (Reduction _ dco2 ty2)
  = Reduction ty1 (dco1 `mkTransDCo` dco2) ty2
{-# INLINE mkTransRedn #-}

-- | The reflexive reduction.
mkReflRedn :: Type -> Reduction
mkReflRedn ty = mkReduction ty mkReflDCo ty
{-# INLINE mkReflRedn #-}

-- | Create a 'Reduction' from a kind cast, in which
-- the casted type is the rewritten type.
--
-- Given @ty :: k1@, @mco :: k1 ~ k2@,
-- produces the 'Reduction' @ty ~res_co~> (ty |> mco)@
-- at the given 'Role'.
mkGReflRightRedn :: Type -> CoercionN -> Reduction
mkGReflRightRedn ty co
  = mkReduction
      ty
      (mkGReflRightDCo co)
      (mkCastTy ty co)
{-# INLINE mkGReflRightRedn #-}

-- | Create a 'Reduction' from a kind cast, in which
-- the casted type is the rewritten type.
--
-- Given @ty :: k1@, @mco :: k1 ~ k2@,
-- produces the 'Reduction' @ty ~res_co~> (ty |> mco)@
-- at the given 'Role'.
mkGReflRightMRedn :: Type -> MCoercionN -> Reduction
mkGReflRightMRedn ty MRefl
  = mkReflRedn ty
mkGReflRightMRedn ty (MCo kco)
  = mkReduction
      ty
      (mkGReflRightDCo kco)
      (mkCastTy ty kco)
{-# INLINE mkGReflRightMRedn #-}

-- | Create a 'Reduction' from a kind cast, in which
-- the casted type is the original (non-rewritten) type.
--
-- Given @ty :: k1@, @mco :: k1 ~ k2@,
-- produces the 'Reduction' @(ty |> mco) ~res_co~> ty@
-- at the given 'Role'.
mkGReflLeftRedn :: Type -> CoercionN -> Reduction
mkGReflLeftRedn ty co
  = mkReduction
      (mkCastTy ty co)
      (mkGReflLeftDCo co)
      ty
{-# INLINE mkGReflLeftRedn #-}

-- | Create a 'Reduction' from a kind cast, in which
-- the casted type is the original (non-rewritten) type.
--
-- Given @ty :: k1@, @mco :: k1 ~ k2@,
-- produces the 'Reduction' @(ty |> mco) ~res_co~> ty@
-- at the given 'Role'.
mkGReflLeftMRedn :: Type -> MCoercionN -> Reduction
mkGReflLeftMRedn ty MRefl
  = mkReflRedn ty
mkGReflLeftMRedn ty (MCo kco)
  = mkReduction
      (mkCastTy ty kco)
      (mkGReflLeftDCo kco)
      ty
{-# INLINE mkGReflLeftMRedn #-}

-- | Apply a cast to the result of a 'Reduction'.
--
-- Given a 'Reduction' @ty1 ~co1~> (ty2 :: k2)@ and a kind coercion @kco@
-- with LHS kind @k2@, produce a new 'Reduction' @ty1 ~co2~> ( ty2 |> kco )@
-- of the given 'Role' (which must match the role of the coercion stored
-- in the 'Reduction' argument).
mkCoherenceRightRedn :: Reduction -> CoercionN -> Reduction
mkCoherenceRightRedn (Reduction ty1 co1 ty2) kco
  = mkReduction
      ty1
      (mkCoherenceRightDCo kco co1)
      (mkCastTy ty2 kco)
{-# INLINE mkCoherenceRightRedn #-}

-- | Apply a cast to the result of a 'Reduction', using an 'MCoercionN'.
--
-- Given a 'Reduction' @ty1 ~co1~> (ty2 :: k2)@ and a kind coercion @mco@
-- with LHS kind @k2@, produce a new 'Reduction' @ty1 ~co2~> ( ty2 |> mco )@
-- of the given 'Role' (which must match the role of the coercion stored
-- in the 'Reduction' argument).
mkCoherenceRightMRedn :: Reduction -> MCoercionN -> Reduction
mkCoherenceRightMRedn redn MRefl = redn
mkCoherenceRightMRedn (Reduction ty1 co1 ty2) (MCo kco)
  = mkReduction
      ty1
      (mkCoherenceRightDCo kco co1)
      (mkCastTy ty2 kco)
{-# INLINE mkCoherenceRightMRedn #-}

-- | Apply a cast to a 'Reduction', casting both the original and the reduced type.
--
-- Given @cast_co@ and 'Reduction' @ty ~co~> xi@, this function returns
-- the 'Reduction' @(ty |> cast_co) ~return_co~> (xi |> cast_co)@
-- of the given 'Role' (which must match the role of the coercion stored
-- in the 'Reduction' argument).
--
-- Pre-condition: the 'Type' passed in is the same as the LHS type
-- of the coercion stored in the 'Reduction'.
mkCastRedn1 :: CoercionN -- ^ coercion to cast with
            -> Reduction  -- ^ rewritten type, with rewriting coercion
            -> Reduction
mkCastRedn1 cast_co (Reduction ty dco xi)
  -- co :: ty ~r ty'
  -- return_co :: (ty |> cast_co) ~r (ty' |> cast_co)
  = mkReduction
      (mkCastTy ty cast_co)
      (castDCoercionKind1 dco cast_co)
      (mkCastTy xi cast_co)
{-# INLINE mkCastRedn1 #-}

-- | Apply casts on both sides of a 'Reduction' (of the given 'Role').
--
-- Use 'mkCastRedn1' when you want to cast both the original and reduced types
-- in a 'Reduction' using the same coercion.
--
-- Pre-condition: the 'Type' passed in is the same as the LHS type
-- of the coercion stored in the 'Reduction'.
mkCastRedn2 :: CoercionN -- ^ coercion to cast with on the left
            -> Reduction -- ^ rewritten type, with rewriting coercion
            -> CoercionN -- ^ coercion to cast with on the right
            -> Reduction
mkCastRedn2 cast_co (Reduction ty nco nty) cast_co'
  = mkReduction
      (mkCastTy ty cast_co)
      (castDCoercionKind2 nco cast_co cast_co')
      (mkCastTy nty cast_co')
{-# INLINE mkCastRedn2 #-}

-- | Apply one 'Reduction' to another.
--
-- Combines 'mkAppCo' and 'mkAppTy`.
mkAppRedn :: Reduction -> Reduction -> Reduction
mkAppRedn (Reduction lty1 co1 rty1) (Reduction lty2 co2 rty2)
  = mkReduction
      (mkAppTy lty1 lty2)
      (mkAppDCo co1 co2)
      (mkAppTy rty1 rty2)
{-# INLINE mkAppRedn #-}

-- | Create a function 'Reduction'.
--
-- Combines 'mkFunCo' and 'mkFunTy'.
mkFunRedn :: AnonArgFlag
          -> ReductionN -- ^ multiplicity reduction
          -> DCoercionN -- ^ argument 'RuntimeRep' coercion
          -> DCoercionN -- ^ result 'RuntimeRep' coercion
          -> Reduction  -- ^ argument reduction
          -> Reduction  -- ^ result reduction
          -> Reduction
mkFunRedn vis
  (Reduction w_lty w_co w_rty)
  arg_repco
  res_repco
  (Reduction arg_lty arg_co arg_rty)
  (Reduction res_lty res_co res_rty)
    = mkReduction
        (mkFunTy vis w_lty arg_lty res_lty)
        (mkFunDCo w_co arg_repco res_repco arg_co res_co)
        (mkFunTy vis w_rty arg_rty res_rty)
{-# INLINE mkFunRedn #-}

-- | Create a 'Reduction' associated to a Π type,
-- from a kind 'Reduction' and a body 'Reduction'.
--
-- Combines 'mkForAllCo' and 'mkForAllTy'.
mkForAllRedn :: ArgFlag
             -> TyVar
             -> ReductionN -- ^ kind reduction
             -> Reduction  -- ^ body reduction
             -> Reduction
mkForAllRedn vis tv1 (Reduction _ h rki) (Reduction lty co rty)
  = mkReduction
      (mkForAllTy tv1 vis lty)
      (mkForAllDCo tv1 h co)
      (mkForAllTy tv2 vis rty)
  where
    tv2 = setTyVarKind tv1 rki
{-# INLINE mkForAllRedn #-}

-- | Create a 'Reduction' of a quantified type from a
-- 'Reduction' of the body.
--
-- Combines 'mkHomoForAllCos' and 'mkForAllTys'.
mkHomoForAllRedn :: [TyVarBinder] -> Reduction -> Reduction
mkHomoForAllRedn bndrs (Reduction ty1 co ty2)
  = mkReduction
      (mkForAllTys bndrs ty1)
      (mkHomoForAllDCos (binderVars bndrs) co)
      (mkForAllTys bndrs ty2)
{-# INLINE mkHomoForAllRedn #-}

-- | Create a 'Reduction' from a coercion between coercions.
--
-- Combines 'mkProofIrrelCo' and 'mkCoercionTy'.
mkProofIrrelRedn :: Coercion   -- ^ lhs_co
                 -> DCoercionN -- ^ dco :: lhs_co ~ rhs_co
                 -> Coercion   -- ^ rhs_co
                 -> Reduction
mkProofIrrelRedn g1 co g2
  = mkReduction
      lhs_co
      (mkProofIrrelDCo co rhs_co)
      rhs_co
  where
    lhs_co = mkCoercionTy g1
    rhs_co = mkCoercionTy g2
{-# INLINE mkProofIrrelRedn #-}

-- | Create a reflexive 'Reduction' whose LHS and RHS is the given 'Coercion',
-- with the specified 'Role'.
mkReflCoRedn :: Coercion -> Reduction
mkReflCoRedn co = mkReduction co_ty mkReflDCo co_ty
  where
    co_ty = mkCoercionTy co
{-# INLINE mkReflCoRedn #-}

-- | A collection of 'Reduction's where the coercions and the types are stored separately.
--
-- Use 'unzipRedns' to obtain 'Reductions' from a list of 'Reduction's.
--
-- This datatype is used in 'mkAppRedns', 'mkClassPredRedns' and 'mkTyConAppRedn',
-- which expect separate types and coercions.
--
-- Invariant: given @Reductions lhs_tys dcos rhs_tys@, and an ambient role @r@,
-- we can obtain the @rhs_tys@ by following the directed coercions starting from the repsective
-- @lhs_tys@. Equivalent, @zipWith (followDCo r) lhs_tys dcos@ is equal (up to zonking) to @rhs_tys@.
data Reductions = Reductions [Type] [DCoercion] [Type]

instance Outputable Reductions where
  ppr (Reductions ltys dcos rtys) = parens (text "Reductions" <+> ppr ltys <+> ppr dcos <+> ppr rtys)

-- | Create 'Reductions' from individual lists of coercions and types.
--
-- The lists should be of the same length, and the RHS type of each coercion
-- should match the specified type in the other list.
mkReductions :: [Type] -> [DCoercion] -> [Type] -> Reductions
mkReductions tys1 cos tys2 = Reductions tys1 cos tys2
{-# INLINE mkReductions #-}

mkReflRedns :: [Type] -> Reductions
mkReflRedns tys = mkReductions tys (mkReflDCos tys) tys
{-# INLINE mkReflRedns #-}

mkReflDCos :: [Type] -> [DCoercion]
mkReflDCos tys = replicate (length tys) mkReflDCo
{-# INLINE mkReflDCos #-}

-- | Combines 'mkAppCos' and 'mkAppTys'.
mkAppRedns :: Reduction -> Reductions -> Reduction
mkAppRedns (Reduction ty1 co ty2) (Reductions tys1 cos tys2)
  = mkReduction (mkAppTys ty1 tys1) (mkAppDCos co cos) (mkAppTys ty2 tys2)
{-# INLINE mkAppRedns #-}

-- | 'TyConAppCo' for 'Reduction's: combines 'mkTyConAppCo' and `mkTyConApp`.
--
-- Use this when you know the 'TyCon' is not a type synonym. If it might be,
-- use 'mkTyConAppRedn_MightBeSynonym'.
mkTyConAppRedn :: TyCon -> Reductions -> Reduction
mkTyConAppRedn tc (Reductions tys1 cos tys2)
  = mkReduction (mkTyConApp tc tys1) (mkTyConAppDCo cos) (mkTyConApp tc tys2)
{-# INLINE mkTyConAppRedn #-}

-- | 'TyConAppCo' for 'Reduction's: combines 'mkTyConAppCo' and `mkTyConApp`.
--
-- Use 'mkTyConAppRedn' if the 'TyCon' is definitely not a type synonym.
mkTyConAppRedn_MightBeSynonym :: Role -> TyCon -> Reductions -> Reduction
mkTyConAppRedn_MightBeSynonym role tc redns@(Reductions tys1 dcos tys2)
  -- 'mkTyConAppCo' handles synomyms by using substitution lifting.
  -- We don't have that for directed coercions, so we use hydrate/dehydrate
  -- so that we can call 'liftCoSubst'.
  -- In the future, it might be desirable to implement substitution lifting
  -- for directed coercions to avoid this (and a similar issue in simplifyArgsWorker).
  | Just (tv_dco_prs, rhs_ty, leftover_dcos) <- expandSynTyCon_maybe tc dcos
  , let tv_co_prs = zipWith4 hydrate (tyConRolesX role tc) tys1 tv_dco_prs tys2
  = mkReduction
      (mkTyConApp tc tys1)
      (mkAppDCos (mkDehydrateCo $ liftCoSubst role (mkLiftingContext tv_co_prs) rhs_ty) leftover_dcos)
      (mkTyConApp tc tys2)
  | otherwise = mkTyConAppRedn tc redns
  where
    hydrate r l (tv,dco) t = (tv, mkHydrateDCo r l dco (Just t))
    -- N.B.: we are hydrating using the LHS argument types,
    -- which are stored in 'Reductions'.
    -- This upholds the necessary hydration invariant from
    -- Note [The Hydration invariant] in GHC.Core.Coercion.
    {-# INLINE hydrate #-}
{-# INLINE mkTyConAppRedn_MightBeSynonym #-}

-- | Reduce the arguments of a 'Class' 'TyCon'.
mkClassPredRedn :: Class -> Reductions -> Reduction
mkClassPredRedn cls (Reductions tys1 cos tys2)
  = mkReduction
      (mkClassPred cls tys1)
      (mkTyConAppDCo cos)
      (mkClassPred cls tys2)
{-# INLINE mkClassPredRedn #-}

-- | Obtain 'Reductions' from a list of 'Reduction's by unzipping.
unzipRedns :: [Reduction] -> Reductions
unzipRedns = foldr accRedn (Reductions [] [] [])
  where
    accRedn :: Reduction -> Reductions -> Reductions
    accRedn (Reduction ty co xi) (Reductions tys cos xis)
      = Reductions (ty:tys) (co:cos) (xi:xis)
{-# INLINE unzipRedns #-}
-- NB: this function is currently used in two locations:
--
-- - GHC.Tc.Gen.Foreign.normaliseFfiType', with one call of the form:
--
--   unzipRedns <$> zipWithM f tys roles
--
-- - GHC.Tc.Solver.Monad.breakTyVarCycle_maybe, with two calls of the form:
--
--   unzipRedns <$> mapM f tys
--
-- It is possible to write 'mapAndUnzipM' functions to handle these cases,
-- but the above locations aren't performance critical, so it was deemed
-- to not be worth it.

-- | Stores a heterogeneous reduction.
--
-- The stored kind coercion must relate the kinds of the
-- stored reduction. That is, in @HetReduction (Reduction co xi) kco@,
-- we must have:
--
-- >  co :: ty ~ xi
-- > kco :: typeKind ty ~ typeKind xi
data HetReduction =
  HetReduction
    Reduction
    MCoercionN
  -- N.B. strictness annotations don't seem to make a difference here

-- | Create a heterogeneous reduction.
--
-- Pre-condition: the provided kind coercion (second argument)
-- relates the kinds of the stored reduction.
-- That is, if the coercion stored in the 'Reduction' is of the form
--
-- > co :: ty ~ xi
--
-- Then the kind coercion supplied must be of the form:
--
-- > kco :: typeKind ty ~ typeKind xi
mkHetReduction :: Reduction  -- ^ heterogeneous reduction
               -> MCoercionN -- ^ kind coercion
               -> HetReduction
mkHetReduction redn mco = HetReduction redn mco
{-# INLINE mkHetReduction #-}

-- | Homogenise a heterogeneous reduction.
--
-- Given @HetReduction (Reduction co xi) kco@, with
--
-- >  co :: ty ~ xi
-- > kco :: typeKind(ty) ~ typeKind(xi)
--
-- this returns the homogeneous reduction:
--
-- > hco :: ty ~ ( xi |> sym kco )
homogeniseHetRedn :: HetReduction -> Reduction
homogeniseHetRedn (HetReduction redn kco)
  = mkCoherenceRightMRedn redn (mkSymMCo kco)
{-# INLINE homogeniseHetRedn #-}

-- | Homogenise a reduction.
--
-- Given @redn = Reduction co xi@ and kind coercion @kco@, with
--
-- >  co :: ty ~ xi
-- > kco :: typeKind(ty) ~ typeKind(xi)
--
-- this returns the homogeneous reduction:
--
-- > hco :: ty ~ ( xi |> sym kco )
homogeniseRedn :: Reduction -> MCoercionN -> Reduction
homogeniseRedn redn mco
  = mkCoherenceRightMRedn redn (mkSymMCo mco)
{-# INLINE homogeniseRedn #-}

{-
%************************************************************************
%*                                                                      *
       Simplifying types
%*                                                                      *
%************************************************************************

The function below morally belongs in GHC.Tc.Solver.Rewrite, but it is used also in
FamInstEnv, and so lives here.

Note [simplifyArgsWorker]
~~~~~~~~~~~~~~~~~~~~~~~~~
Invariant (F2) of Note [Rewriting] in GHC.Tc.Solver.Rewrite says that
rewriting is homogeneous.
This causes some trouble when rewriting a function applied to a telescope
of arguments, perhaps with dependency. For example, suppose

  type family F :: forall (j :: Type) (k :: Type). Maybe j -> Either j k -> Bool -> [k]

and we wish to rewrite the args of (with kind applications explicit)

  F @a @b (Just @a c) (Right @a @b d) False

where all variables are skolems and

  a :: Type
  b :: Type
  c :: a
  d :: b

  [G] aco :: a ~ fa
  [G] bco :: b ~ fb
  [G] cco :: c ~ fc
  [G] dco :: d ~ fd

The first step is to rewrite all the arguments. This is done before calling
simplifyArgsWorker. We start from

  a
  b
  Just @a c
  Right @a @b d
  False

and get left-to-right reductions whose coercions are as follows:

  co1 :: a ~ fa
  co2 :: b ~ fb
  co3 :: (Just @a c) ~ (Just @fa (fc |> aco) |> co6)
  co4 :: (Right @a @b d) ~ (Right @fa @fb (fd |> bco) |> co7)
  co5 :: False ~ False

where
  co6 = Maybe (sym aco) :: Maybe fa ~ Maybe a
  co7 = Either (sym aco) (sym bco) :: Either fa fb ~ Either a b

We now process the rewritten args in left-to-right order. The first two args
need no further processing. But now consider the third argument. Let f3 = the rewritten
result, Just fa (fc |> aco) |> co6.
This f3 rewritten argument has kind (Maybe a), due to homogeneity of rewriting (F2).
And yet, when we build the application (F @fa @fb ...), we need this
argument to have kind (Maybe fa), not (Maybe a). We must cast this argument.
The coercion to use is determined by the kind of F:
we see in F's kind that the third argument has kind Maybe j.
Critically, we also know that the argument corresponding to j
(in our example, a) rewrote with a coercion co1. We can thus know the
coercion needed for the 3rd argument is (Maybe co1), thus building
(f3 |> Maybe co1)

More generally, we must use the Lifting Lemma, as implemented in
Coercion.liftCoSubst. As we work left-to-right, any variable that is a
dependent parameter (j and k, in our example) gets mapped in a lifting context
to the coercion that is output from rewriting the corresponding argument (co1
and co2, in our example). Then, after rewriting later arguments, we lift the
kind of these arguments in the lifting context that we've be building up.
This coercion is then used to keep the result of rewriting well-kinded.

Working through our example, this is what happens:

  1. Extend the (empty) LC with [j |-> co1]. No new casting must be done,
     because the binder associated with the first argument has a closed type (no
     variables).

  2. Extend the LC with [k |-> co2]. No casting to do.

  3. Lifting the kind (Maybe j) with our LC
     yields co8 :: Maybe a ~ Maybe fa. Use (f3 |> co8) as the argument to F.

  4. Lifting the kind (Either j k) with our LC
     yields co9 :: Either a b ~ Either fa fb. Use (f4 |> co9) as the 4th
     argument to F, where f4 is the rewritten form of argument 4, written above.

  5. We lift Bool with our LC, getting <Bool>; casting has no effect.

We're now almost done, but the new application

  F @fa @fb (f3 |> co8) (f4 |> co9) False

has the wrong kind. Its kind is [fb], instead of the original [b].
So we must use our LC one last time to lift the result kind [k],
getting res_co :: [fb] ~ [b], and we cast our result.

Accordingly, the final result is

  F
    @fa
    @fb
    (Just @fa (fc |> aco) |> Maybe (sym aco) |> sym (Maybe (sym aco)))
    (Right @fa @fb (fd |> bco) |> Either (sym aco) (sym bco) |> sym (Either (sym aco) (sym bco)))
    False
  |> [sym bco]

The res_co (in this case, [sym bco]) is the third component of the
tuple returned by simplifyArgsWorker.

Note [Last case in simplifyArgsWorker]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In writing simplifyArgsWorker's `go`, we know here that args cannot be empty,
because that case is first. We've run out of
binders. But perhaps inner_ki is a tyvar that has been instantiated with a
Π-type.

Here is an example.

  a :: forall (k :: Type). k -> k
  Proxy :: forall j. j -> Type
  type family Star
  axStar :: Star ~ Type
  type family NoWay :: Bool
  axNoWay :: NoWay ~ False
  bo :: Type
  [G] bc :: bo ~ Bool   (in inert set)

  co :: (forall j. j -> Type) ~ (forall (j :: Star). (j |> axStar) -> Star)
  co = forall (j :: sym axStar). (<j> -> sym axStar)

  We are rewriting:
  a (forall (j :: Star). (j |> axStar) -> Star)   -- 1
    (Proxy |> co)                                 -- 2
    (bo |> sym axStar)                            -- 3
    (NoWay |> sym bc)                             -- 4
      :: Star

First, we rewrite all the arguments (before simplifyArgsWorker), like so:

    co1 :: (forall (j :: Star). (j |> axStar) -> Star) ~ (forall j. j -> Type) -- 1
    co2 :: (Proxy |> co) ~ (Proxy |> co)                                       -- 2
    co3 :: (bo |> sym axStar) ~ (Bool |> sym axStar)                           -- 3
    co4 :: (NoWay |> sym bc) ~ (False |> sym bc)                               -- 4

Then we do the process described in Note [simplifyArgsWorker].

1. Lifting Type (the kind of the first arg) gives us a reflexive coercion, so we
   don't use it. But we do build a lifting context [k -> co1] (where co1 is a
   result of rewriting an argument, written above).

2. Lifting k gives us co1, so the second argument becomes (Proxy |> co |> co1).
   This is not a dependent argument, so we don't extend the lifting context.

Now we need to deal with argument (3).
The way we normally proceed is to lift the kind of the binder, to see whether
it's dependent.
But here, the remainder of the kind of `a` that we're left with
after processing two arguments is just `k`.

The way forward is look up k in the lifting context, getting co1. If we're at
all well-typed, co1 will be a coercion between Π-types, with at least one binder.
So, let's decompose co1 with decomposePiCos. This decomposition needs arguments to use
to instantiate any kind parameters. Look at the type of co1. If we just
decomposed it, we would end up with coercions whose types include j, which is
out of scope here. Accordingly, decomposePiCos takes a list of types whose
kinds are the *unrewritten* types in the decomposed coercion. (See comments on
decomposePiCos.) Because the rewritten types have unrewritten kinds (because
rewriting is homogeneous), passing the list of rewritten types to decomposePiCos
just won't do: later arguments' kinds won't be as expected. So we need to get
the *unrewritten* types to pass to decomposePiCos. We can do this easily enough
by taking the kind of the argument coercions, passed in originally.

(Alternative 1: We could re-engineer decomposePiCos to deal with this situation.
But that function is already gnarly, and other call sites of decomposePiCos
would suffer from the change, even though they are much more common than this one.)

(Alternative 2: We could avoid calling decomposePiCos entirely, integrating its
behavior into simplifyArgsWorker. This would work, I think, but then all of the
complication of decomposePiCos would end up layered on top of all the complication
here. Please, no.)

(Alternative 3: We could pass the unrewritten arguments into simplifyArgsWorker
so that we don't have to recreate them. But that would complicate the interface
of this function to handle a very dark, dark corner case. Better to keep our
demons to ourselves here instead of exposing them to callers. This decision is
easily reversed if there is ever any performance trouble due to the call of
coercionKind.)

So we now call

  decomposePiCos co1
                 (Pair (forall (j :: Star). (j |> axStar) -> Star) (forall j. j -> Type))
                 [bo |> sym axStar, NoWay |> sym bc]

to get

  co5 :: Star ~ Type
  co6 :: (j |> axStar) ~ (j |> co5), substituted to
                              (bo |> sym axStar |> axStar) ~ (bo |> sym axStar |> co5)
                           == bo ~ bo
  res_co :: Type ~ Star

We then use these casts on (the rewritten) (3) and (4) to get

  (Bool |> sym axStar |> co5 :: Type)   -- (C3)
  (False |> sym bc |> co6    :: bo)     -- (C4)

We can simplify to

  Bool                        -- (C3)
  (False |> sym bc :: bo)     -- (C4)

Of course, we still must do the processing in Note [simplifyArgsWorker] to finish
the job. We thus want to recur. Our new function kind is the left-hand type of
co1 (gotten, recall, by lifting the variable k that was the return kind of the
original function). Why the left-hand type (as opposed to the right-hand type)?
Because we have casted all the arguments according to decomposePiCos, which gets
us from the right-hand type to the left-hand one. We thus recur with that new
function kind, zapping our lifting context, because we have essentially applied
it.

This recursive call returns ([Bool, False], [...], Refl). The Bool and False
are the correct arguments we wish to return. But we must be careful about the
result coercion: our new, rewritten application will have kind Type, but we
want to make sure that the result coercion casts this back to Star. (Why?
Because we started with an application of kind Star, and rewriting is homogeneous.)

So, we have to twiddle the result coercion appropriately.

Let's check whether this is well-typed. We know

  a :: forall (k :: Type). k -> k

  a (forall j. j -> Type) :: (forall j. j -> Type) -> forall j. j -> Type

  a (forall j. j -> Type)
    Proxy
      :: forall j. j -> Type

  a (forall j. j -> Type)
    Proxy
    Bool
      :: Bool -> Type

  a (forall j. j -> Type)
    Proxy
    Bool
    False
      :: Type

  a (forall j. j -> Type)
    Proxy
    Bool
    False
     |> res_co
     :: Star

as desired.

Whew.

Historical note: I (Richard E) once thought that the final part of the kind
had to be a variable k (as in the example above). But it might not be: it could
be an application of a variable. Here is the example:

  let f :: forall (a :: Type) (b :: a -> Type). b (Any @a)
      k :: Type
      x :: k

  rewrite (f @Type @((->) k) x)

After instantiating [a |-> Type, b |-> ((->) k)], we see that `b (Any @a)`
is `k -> Any @a`, and thus the third argument of `x :: k` is well-kinded.

-}

-- | Stores 'Reductions' as well as a kind coercion.
--
-- Used when rewriting arguments to a type function @f@.
--
-- Invariant:
--   when the stored reductions are of the form
--     co_i :: ty_i ~ xi_i,
--   the kind coercion is of the form
--      kco :: typeKind (f ty_1 ... ty_n) ~ typeKind (f xi_1 ... xi_n)
--
-- The type function @f@ depends on context.
data ArgsReductions =
  ArgsReductions
    {-# UNPACK #-} !Reductions
    !MCoercionN
  -- The strictness annotations and UNPACK pragma here are crucial
  -- to getting good performance in simplifyArgsWorker's tight loop.

instance Outputable ArgsReductions where
  ppr (ArgsReductions redns kco) = parens (text "ArgsReductions" <+> ppr redns <+> ppr kco)

-- This is shared between the rewriter and the normaliser in GHC.Core.FamInstEnv.
-- See Note [simplifyArgsWorker]
{-# INLINE simplifyArgsWorker #-}
-- NB. INLINE yields a ~1% decrease in allocations in T9872d compared to INLINEABLE
-- This function is only called in two locations, so the amount of code duplication
-- should be rather reasonable despite the size of the function.
simplifyArgsWorker :: HasDebugCallStack
                   => [TyCoBinder] -> Kind
                       -- the binders & result kind (not a Π-type) of the function applied to the args
                       -- list of binders can be shorter or longer than the list of args
                   -> TyCoVarSet   -- free vars of the args
                   -> [Role]       -- list of roles, r
                   -> [Reduction]  -- rewritten type arguments, arg_i
                                   -- each comes with the coercion used to rewrite it,
                                   -- arg_co_i :: ty_i ~ arg_i
                   -> ArgsReductions
-- Returns ArgsReductions (Reductions cos xis) res_co, where co_i :: ty_i ~ xi_i,
-- and res_co :: kind (f ty_1 ... ty_n) ~ kind (f xi_1 ... xi_n), where f is the function
-- that we are applying.
-- Precondition: if f :: forall bndrs. inner_ki (where bndrs and inner_ki are passed in),
-- then (f ty_1 ... ty_n) is well kinded. Note that (f arg_1 ... arg_n) might *not* be well-kinded.
-- Massaging the arg_i in order to make the function application well-kinded is what this
-- function is all about. That is, (f xi_1 ... xi_n), where xi_i are the returned arguments,
-- *is* well kinded.
simplifyArgsWorker orig_ki_binders orig_inner_ki orig_fvs
                   orig_roles redns
  = go orig_lc
       orig_ki_binders orig_inner_ki
       orig_roles redns
  where
    orig_lc = emptyLiftingContext $ mkInScopeSet orig_fvs

    go :: LiftingContext  -- mapping from tyvars to rewriting coercions
       -> [TyCoBinder]    -- Unsubsted binders of function's kind
       -> Kind        -- Unsubsted result kind of function (not a Pi-type)
       -> [Role]      -- Roles at which to rewrite these ...
       -> [Reduction] -- rewritten arguments, with their rewriting coercions
       -> ArgsReductions
    go !lc binders inner_ki _ []
        -- The !lc makes the function strict in the lifting context
        -- which means GHC can unbox that pair.  A modest win.
      = ArgsReductions
          (mkReductions [] [] [])
          kind_co
      where
        final_kind = mkPiTys binders inner_ki
        kind_co | noFreeVarsOfType final_kind = MRefl
                | otherwise                   = MCo $ liftCoSubst Nominal lc final_kind

    go lc (binder:binders) inner_ki (role:roles) (arg_redn:arg_redns)
      =  -- We rewrite an argument ty with arg_redn = Reduction arg_co arg
         -- By Note [Rewriting] in GHC.Tc.Solver.Rewrite invariant (F2),
         -- tcTypeKind(ty) = tcTypeKind(arg).
         -- However, it is possible that arg will be used as an argument to a function
         -- whose kind is different, if earlier arguments have been rewritten.
         -- We thus need to compose the reduction with a kind coercion to ensure
         -- well-kindedness (see the call to mkCoherenceRightRedn below).
         --
         -- The bangs here have been observed to improve performance
         -- significantly in optimized builds; see #18502
         let !kind_co = liftCoSubst Nominal lc (tyCoBinderType binder)
             !(Reduction arg_ty casted_co casted_xi)
                      = mkCoherenceRightRedn arg_redn kind_co
         -- now, extend the lifting context with the new binding
             !new_lc | Just tv <- tyCoBinderVar_maybe binder
                     = extendLiftingContextAndInScope lc tv
                       (mkHydrateDCo role arg_ty casted_co (Just casted_xi))
                     -- NB: this is the crucial place where we need the hydration invariant,
                     -- which is satisfied here as we use the LHS type stored in a 'Reduction'.
                     -- See Note [The Reduction type], as well as
                     -- Note [The Hydration invariant] in GHC.Core.Coercion.
                     -- This could be avoided if we had substitution lifting for directed coercions.
                     -- See also mkTyConAppRedn_MightBeSynonym, which is the other place where
                     -- we need this.
                     | otherwise
                     = lc
             !(ArgsReductions (Reductions arg_tys cos xis) final_kind_co)
               = go new_lc binders inner_ki roles arg_redns
         in ArgsReductions
              (Reductions (arg_ty:arg_tys) (casted_co:cos) (casted_xi:xis))
              final_kind_co

    -- See Note [Last case in simplifyArgsWorker]
    go lc [] inner_ki roles arg_redns
      = let co1 = liftCoSubst Nominal lc inner_ki
            co1_kind              = coercionKind co1
            (arg_cos, res_co)     = decomposePiCos co1 co1_kind (map reductionOriginalType arg_redns)
            casted_args           = assertPpr (equalLength arg_redns arg_cos)
                                              (ppr arg_redns $$ ppr arg_cos)
                                  $ zipWith mkCoherenceRightRedn arg_redns arg_cos
               -- In general decomposePiCos can return fewer cos than tys,
               -- but not here; because we're well typed, there will be enough
               -- binders. Note that decomposePiCos does substitutions, so even
               -- if the original substitution results in something ending with
               -- ... -> k, that k will be substituted to perhaps reveal more
               -- binders.
            zapped_lc             = zapLiftingContext lc
            Pair rewritten_kind _ = co1_kind
            (bndrs, new_inner)    = splitPiTys rewritten_kind

            ArgsReductions redns_out res_co_out
              = go zapped_lc bndrs new_inner roles casted_args
        in
          ArgsReductions redns_out (res_co `mkTransMCoR` res_co_out)

    go _ _ _ _ _ = panic
        "simplifyArgsWorker wandered into deeper water than usual"
           -- This debug information is commented out because leaving it in
           -- causes a ~2% increase in allocations in T9872d.
           -- That's independent of the analogous case in rewrite_args_fast
           -- in GHC.Tc.Solver.Rewrite:
           -- each of these causes a 2% increase on its own, so commenting them
           -- both out gives a 4% decrease in T9872d.
           {-

             (vcat [ppr orig_binders,
                    ppr orig_inner_ki,
                    ppr (take 10 orig_roles), -- often infinite!
                    ppr orig_tys])
           -}
