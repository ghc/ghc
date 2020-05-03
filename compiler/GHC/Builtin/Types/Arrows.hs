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
                         , arrowStackTupTyFamKey
                         , arrowEnvTupTyFamKey )
import GHC.Builtin.Types
import GHC.Builtin.Types.Prim ( mkTemplateAnonTyConBinders, alphaTyVar, alphaTy )
import GHC.Core.Coercion
import GHC.Core.Coercion.Axiom
import GHC.Core.FamInstEnv
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Data.FastString ( fsLit )
import GHC.Data.Pair
import GHC.Tc.Utils.TcType ( tcEqType )
import GHC.Types.Name

arrowTyCons :: [TyCon]
arrowTyCons = [arrowEnvTyCon, arrowStackTupTyCon, arrowEnvTupTyCon]

arrowCoAxiomRules :: [CoAxiomRule]
arrowCoAxiomRules = [axArrowStackTupDef, axArrowEnvTupDef]

arrowEnvTyCon :: TyCon
arrowEnvCoAxiom :: CoAxiom Unbranched
(arrowEnvTyCon, arrowEnvCoAxiom) = (ty_con, co_ax)
  where
    ty_con = mkAlgTyCon name
                        (mkAnonTyConBinders VisArg tvs)
                        liftedTypeKind
                        [Representational]
                        Nothing
                        []              -- No stupid theta
                        rhs
                        (VanillaAlgTyCon (mkPrelTyConRepName name))
                        False           -- Not in GADT syntax

    name   = mkWiredInTyConName UserSyntax gHC_DESUGAR (fsLit "ArrowEnv")
               arrowEnvTyConKey arrowEnvTyCon
    tvs    = [alphaTyVar]
    rhs_ty = alphaTy
    rhs    = NewTyCon data_con rhs_ty (tvs, rhs_ty) co_ax False

    co_ax_name = mkWiredInName gHC_DESUGAR (mkNewTyCoOcc (nameOccName name))
                   arrowEnvCoAxiomKey (ACoAxiom (toBranchedAxiom co_ax)) UserSyntax
    co_ax      = mkNewTypeCoAxiom co_ax_name arrowEnvTyCon tvs
                                  [Representational] rhs_ty

    data_con  = pcDataCon data_name tvs [rhs_ty] arrowEnvTyCon
    data_name = mkWiredInDataConName UserSyntax gHC_DESUGAR (fsLit "ArrowEnv")
                  arrowEnvDataConKey data_con

mkArrowEnvTy :: Type -> Type
mkArrowEnvTy ty = mkTyConApp arrowEnvTyCon [ty]

mkArrowEnvCo :: Type -> CoercionR
mkArrowEnvCo ty = mkUnbranchedAxInstCo Representational arrowEnvCoAxiom [ty] []

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
             arrowStackTupTyFamKey arrowStackTupTyCon
    tcb = BuiltInSynFamily
      { sfMatchFam      = matchFamArrowStackTup
      , sfInteractTop   = \_ _ -> []
      , sfInteractInert = \_ _ _ _ -> [] }

    matchFamArrowStackTup tys = do
      [stk_ty] <- pure tys
      stk_tys <- isPromotedListTy stk_ty
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
      tys2 <- isPromotedListTy ty2
      pure (mkArrowStackTupTy ty1 `Pair` mkBoxedTupleTy tys2)
  }

mkArrowStackTupCo :: [Type] -> CoercionN
mkArrowStackTupCo stk_tys
  = mkAxiomRuleCo axArrowStackTupDef
                  [ mkNomReflCo $ mkPromotedListTy liftedTypeKind stk_tys ]

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
             arrowEnvTupTyFamKey arrowEnvTupTyCon
    tcb = BuiltInSynFamily
      { sfMatchFam      = matchFamArrowEnvTup
      , sfInteractTop   = interactTopArrowEnvTup
      , sfInteractInert = interactInertArrowEnvTup }

    matchFamArrowEnvTup tys = do
      [env_ty, stk_ty] <- pure tys
      stk_tys <- isPromotedListTy stk_ty
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
      stk_tys2 <- isPromotedListTy stk_ty2
      let rhs_ty = mkBoxedTupleTy (mkArrowEnvTy env_ty2 : stk_tys2)
      pure (mkArrowEnvTupTy env_ty1 stk_ty1 `Pair` rhs_ty)
  }

mkArrowEnvTupCo :: Type -> [Type] -> CoercionN
mkArrowEnvTupCo env_ty stk_tys
  = mkAxiomRuleCo axArrowEnvTupDef
                  [ mkNomReflCo env_ty
                  , mkNomReflCo $ mkPromotedListTy liftedTypeKind stk_tys ]
