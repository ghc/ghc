module GHC.Builtin.Types.Arrows
  ( arrowTyCons
  , arrowCoAxiomRules

  , arrowEnvTyCon
  , mkArrowEnvTy
  , mkArrowEnvCo
  , arrowStackTupTyCon
  , mkArrowStackTupTy
  , mkArrowStackTupCo
  , arrowEnvTupTyCon
  , mkArrowEnvTupTy
  , mkArrowEnvTupCo
  ) where

import GHC.Prelude

import GHC.Builtin.Names ( gHC_DESUGAR
                         , arrowEnvTyConKey
                         , arrowEnvDataConKey
                         , arrowEnvCoAxiomKey
                         , arrowStackTupTyConKey
                         , arrowEnvTupTyConKey )
import GHC.Builtin.Types
import GHC.Builtin.Types.Prim ( mkTemplateAnonTyConBinders, alphaTyVar, alphaTy )
import GHC.Core.Coercion
import GHC.Core.Coercion.Axiom
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Data.FastString ( fsLit )
import GHC.Data.Pair
import GHC.Tc.Utils.TcType ( tcEqType )
import GHC.Types.Name

{- Note [Arrow type families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module defines two wired-in type families, ArrowStackTup and
ArrowEnvTup, both of which build tuple types from promoted lists:

    type ArrowStackTup :: [Type] -> Type
    type family ArrowStackTup stk where
      ArrowStackTup '[]        = ()
      ArrowStackTup '[a]       = a
      ArrowStackTup '[a, b]    = (a, b)
      ArrowStackTup '[a, b, c] = (a, b, c)
      ...

    type ArrowEnvTup :: Type -> [Type] -> Type
    type family ArrowEnvTup env stk = r | r -> env, stk where
      ArrowEnvTup env '[]        = ArrowEnv env
      ArrowEnvTup env '[a]       = (ArrowEnv env, a)
      ArrowEnvTup env '[a, b]    = (ArrowEnv env, a, b)
      ArrowEnvTup env '[a, b, c] = (ArrowEnv env, a, b, c)
      ...

These type families are used to assist in typechecking arrow notation;
see Note [Command typing] in GHC.Tc.Gen.Arrow.

The definitions of these families live in GHC.Desugar, which also
contains the definition of the ArrowEnv newtype:

    newtype ArrowEnv env = ArrowEnv env

This newtype’s only purpose is to allow ArrowEnvTup to be injective:
without the wrapping its first equation would overlap with all others.
This is crucial, since it allows information about the shape of the
stack to propagate bidirectionally. See Note [Control operator typing]
in GHC.Tc.Gen.Arrow for the details. -}

arrowTyCons :: [TyCon]
arrowTyCons = [arrowEnvTyCon, arrowStackTupTyCon, arrowEnvTupTyCon]

arrowCoAxiomRules :: [CoAxiomRule]
arrowCoAxiomRules = [axArrowStackTupDef, axArrowEnvTupDef]

-- -------------------------------------------------------------------

-- | > newtype ArrowEnv env = ArrowEnv env
--
-- This newtype’s sole purpose is to allow @ArrowEnvTup@ to be
-- injective; see Note [Arrow type families].
arrowEnvTyCon :: TyCon
arrowEnvTyCon = pcNewTyCon ty_name arrowEnvDataConKey arrowEnvCoAxiomKey
                           [alphaTyVar] alphaTy
  where
    ty_name   = mkWiredInTyConName UserSyntax gHC_DESUGAR (fsLit "ArrowEnv")
                  arrowEnvTyConKey arrowEnvTyCon

mkArrowEnvTy :: Type -> Type
mkArrowEnvTy ty = mkTyConApp arrowEnvTyCon [ty]

mkArrowEnvCo :: Type -> CoercionR
mkArrowEnvCo ty
  = mkUnbranchedAxInstCo Representational (newTyConCo arrowEnvTyCon) [ty] []

-- -------------------------------------------------------------------

-- | A wired-in type family used to convert the command stack type to
-- a tuple in the typing rule for arrow application. Has the following
-- infinitely-long definition:
--
-- > type ArrowStackTup :: [Type] -> Type
-- > type family ArrowStackTup stk where
-- >   ArrowStackTup '[]        = ()
-- >   ArrowStackTup '[a]       = a
-- >   ArrowStackTup '[a, b]    = (a, b)
-- >   ArrowStackTup '[a, b, c] = (a, b, c)
-- >   ...
--
-- Also see Note [Arrow type families].
arrowStackTupTyCon :: TyCon
arrowStackTupTyCon =
  mkFamilyTyCon name
    (mkTemplateAnonTyConBinders [ mkListTy liftedTypeKind ])
    liftedTypeKind
    Nothing
    (BuiltInSynFamTyCon tcb)
    Nothing
    NotInjective
  where
    name = mkWiredInTyConName UserSyntax gHC_DESUGAR (fsLit "ArrowStackTup")
             arrowStackTupTyConKey arrowStackTupTyCon
    tcb = BuiltInSynFamily
      { sfMatchFam      = matchFamArrowStackTup
      , sfInteractTop   = \_ _ -> []
      , sfInteractInert = \_ _ _ _ -> [] }

    matchFamArrowStackTup tys = do
      [stk_ty] <- pure tys
      stk_tys <- extractPromotedList_maybe stk_ty
      pure (axArrowStackTupDef, [stk_ty], mkBoxedTupleTy stk_tys)

mkArrowStackTupTy :: Type -> Type
mkArrowStackTupTy stk_ty = mkTyConApp arrowStackTupTyCon [stk_ty]

axArrowStackTupDef :: CoAxiomRule
axArrowStackTupDef = CoAxiomRule
  { coaxrName      = fsLit "ArrowStackTupDef"
  , coaxrAsmpRoles = [Nominal]
  , coaxrRole      = Nominal
  , coaxrProves    = \cs -> do
      [Pair ty1 ty2] <- pure cs
      tys2 <- extractPromotedList_maybe ty2
      pure (mkArrowStackTupTy ty1 `Pair` mkBoxedTupleTy tys2)
  }

mkArrowStackTupCo :: [Type] -> CoercionN
mkArrowStackTupCo stk_tys
  = mkAxiomRuleCo axArrowStackTupDef
                  [ mkNomReflCo $ mkPromotedListTy liftedTypeKind stk_tys ]

-- -------------------------------------------------------------------

-- | A wired-in type family used to convert the command environment
-- and command stack types to a tuple in the typing rule for arrow
-- control operators. Has the following infinitely-long definition:
--
-- > type ArrowEnvTup :: Type -> [Type] -> Type
-- > type family ArrowEnvTup env stk = r | r -> env, stk where
-- >   ArrowEnvTup env '[]        = ArrowEnv env
-- >   ArrowEnvTup env '[a]       = (ArrowEnv env, a)
-- >   ArrowEnvTup env '[a, b]    = (ArrowEnv env, a, b)
-- >   ArrowEnvTup env '[a, b, c] = (ArrowEnv env, a, b, c)
-- >   ...
--
-- Also see Note [Arrow type families] and Note [Control operator typing]
-- in GHC.Tc.Gen.Arrow.
arrowEnvTupTyCon :: TyCon
arrowEnvTupTyCon =
  mkFamilyTyCon name
    (mkTemplateAnonTyConBinders [ liftedTypeKind, mkListTy liftedTypeKind ])
    liftedTypeKind
    Nothing
    (BuiltInSynFamTyCon tcb)
    Nothing
    (Injective [True, True])
  where
    name = mkWiredInTyConName UserSyntax gHC_DESUGAR (fsLit "ArrowEnvTup")
             arrowEnvTupTyConKey arrowEnvTupTyCon
    tcb = BuiltInSynFamily
      { sfMatchFam      = matchFamArrowEnvTup
      , sfInteractTop   = interactTopArrowEnvTup
      , sfInteractInert = interactInertArrowEnvTup }

    matchFamArrowEnvTup tys = do
      [env_ty, stk_ty] <- pure tys
      stk_tys <- extractPromotedList_maybe stk_ty
      let rhs_ty = mkBoxedTupleTy (mkArrowEnvTy env_ty : stk_tys)
      pure (axArrowEnvTupDef, [env_ty, stk_ty], rhs_ty)

    interactTopArrowEnvTup [env_ty, stk_ty] rhs_ty
      --     ArrowEnvTup env stk ~ ArrowEnv env'
      -- ==> env ~ env'
      --     stk ~ '[]
      | Just (env_tc, [env_ty']) <- splitTyConApp_maybe rhs_ty
      , env_tc == arrowEnvTyCon
      = [ mkArrowEnvTy env_ty `Pair` env_ty'
        , stk_ty `Pair` mkPromotedListTy liftedTypeKind [] ]

      --     ArrowEnvTup env stk ~ (env', t1, ..., tn)
      -- ==> ArrowEnv env ~ env'
      --     stk ~ '[t1, ..., tn]
      | Just (tup_tc, env_ty':stk_tys) <- splitTyConApp_maybe rhs_ty
      , isBoxedTupleTyCon tup_tc
      = [ mkArrowEnvTy env_ty `Pair` env_ty'
        , stk_ty `Pair` mkPromotedListTy liftedTypeKind stk_tys ]

    interactTopArrowEnvTup _ _ = []

    interactInertArrowEnvTup [env_ty1, stk_ty1] rhs_ty1
                             [env_ty2, stk_ty2] rhs_ty2
      | rhs_ty1 `tcEqType` rhs_ty2
      = [ env_ty1 `Pair` env_ty2, stk_ty1 `Pair` stk_ty2 ]
    interactInertArrowEnvTup _ _ _ _ = []

mkArrowEnvTupTy :: Type -> Type -> Type
mkArrowEnvTupTy env_ty stk_ty = mkTyConApp arrowEnvTupTyCon [env_ty, stk_ty]

axArrowEnvTupDef :: CoAxiomRule
axArrowEnvTupDef = CoAxiomRule
  { coaxrName      = fsLit "ArrowEnvTupDef"
  , coaxrAsmpRoles = [Nominal, Nominal]
  , coaxrRole      = Nominal
  , coaxrProves    = \cs -> do
      [Pair env_ty1 env_ty2, Pair stk_ty1 stk_ty2] <- pure cs
      stk_tys2 <- extractPromotedList_maybe stk_ty2
      let rhs_ty = mkBoxedTupleTy (mkArrowEnvTy env_ty2 : stk_tys2)
      pure (mkArrowEnvTupTy env_ty1 stk_ty1 `Pair` rhs_ty)
  }

mkArrowEnvTupCo :: Type -> [Type] -> CoercionN
mkArrowEnvTupCo env_ty stk_tys
  = mkAxiomRuleCo axArrowEnvTupDef
                  [ mkNomReflCo env_ty
                  , mkNomReflCo $ mkPromotedListTy liftedTypeKind stk_tys ]
