{-# LANGUAGE Strict #-} -- See Note [Avoiding space leaks in toIface*]

-- | Functions for converting Core things to interface file things.
module GHC.CoreToIface
    ( -- * Binders
      toIfaceTvBndr
    , toIfaceTvBndrs
    , toIfaceIdBndr
    , toIfaceBndr
    , toIfaceTopBndr
    , toIfaceForAllBndr
    , toIfaceForAllBndrs
    , toIfaceTyVar
      -- * Types
    , toIfaceType, toIfaceTypeX
    , toIfaceKind
    , toIfaceTcArgs
    , toIfaceTyCon
    , toIfaceTyCon_name
    , toIfaceTyLit
      -- * Tidying types
    , tidyToIfaceType
    , tidyToIfaceContext
    , tidyToIfaceTcArgs
      -- * Coercions
    , toIfaceCoercion, toIfaceCoercionX
      -- * Pattern synonyms
    , patSynToIfaceDecl
      -- * Expressions
    , toIfaceExpr
    , toIfaceBang
    , toIfaceSrcBang
    , toIfaceLetBndr
    , toIfaceIdDetails
    , toIfaceIdInfo
    , toIfUnfolding
    , toIfaceTickish
    , toIfaceBind
    , toIfaceTopBind
    , toIfaceAlt
    , toIfaceCon
    , toIfaceApp
    , toIfaceVar
      -- * Other stuff
    , toIfaceLFInfo
    , toIfaceBooleanFormula
      -- * CgBreakInfo
    , dehydrateCgBreakInfo
    ) where

import GHC.Prelude

import GHC.StgToCmm.Types

import GHC.ByteCode.Types

import GHC.Core
import GHC.Core.TyCon hiding ( pprPromotionQuote )
import GHC.Core.Coercion.Axiom
import GHC.Core.DataCon
import GHC.Core.Type
import GHC.Core.Multiplicity
import GHC.Core.PatSyn
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Compare( eqType )
import GHC.Core.TyCo.Tidy

import GHC.Builtin.Types.Prim ( eqPrimTyCon, eqReprPrimTyCon )
import GHC.Builtin.Types ( heqTyCon )

import GHC.Iface.Syntax
import GHC.Data.FastString
import GHC.Data.BooleanFormula qualified as BF(BooleanFormula(..))

import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Id.Make ( noinlineIdName, noinlineConstraintIdName )
import GHC.Types.Literal
import GHC.Types.Name
import GHC.Types.Basic
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Tickish
import GHC.Types.Demand ( isNopSig )
import GHC.Types.Cpr ( topCprSig )
import GHC.Types.SrcLoc (unLoc)

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc

import GHC.Hs.Extension (GhcRn)

import Data.Maybe ( isNothing, catMaybes )

{- Note [Avoiding space leaks in toIface*]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Building a interface file depends on the output of the simplifier.
If we build these lazily this would mean keeping the Core AST alive
much longer than necessary causing a space "leak".

This happens for example when we only write the interface file to disk
after code gen has run, in which case we might carry megabytes of core
AST in the heap which is no longer needed.

We avoid this in two ways.
* First we use -XStrict in GHC.CoreToIface which avoids many thunks
  to begin with.
* Second we define NFData instance for Iface syntax and use them to
  force any remaining thunks.

-XStrict is not sufficient as patterns of the form `f (g x)` would still
result in a thunk being allocated for `g x`.

NFData is sufficient for the space leak, but using -XStrict reduces allocation
by ~0.1% when compiling with -O. (nofib/spectral/simple, T10370).
It's essentially free performance hence we use -XStrict on top of NFData.

MR !1633 on gitlab, has more discussion on the topic.
-}

----------------
toIfaceTvBndr :: TyVar -> IfaceTvBndr
toIfaceTvBndr = toIfaceTvBndrX emptyVarSet

toIfaceTvBndrX :: VarSet -> TyVar -> IfaceTvBndr
toIfaceTvBndrX fr tyvar = ( mkIfLclName (occNameFS (getOccName tyvar))
                          , toIfaceTypeX fr (tyVarKind tyvar)
                          )

toIfaceTvBndrs :: [TyVar] -> [IfaceTvBndr]
toIfaceTvBndrs = map toIfaceTvBndr

toIfaceIdBndr :: Id -> IfaceIdBndr
toIfaceIdBndr = toIfaceIdBndrX emptyVarSet

toIfaceIdBndrX :: VarSet -> CoVar -> IfaceIdBndr
toIfaceIdBndrX fr covar = ( toIfaceType (idMult covar)
                          , mkIfLclName (occNameFS (getOccName covar))
                          , toIfaceTypeX fr (varType covar)
                          )

toIfaceBndr :: Var -> IfaceBndr
toIfaceBndr var
  | isId var  = IfaceIdBndr (toIfaceIdBndr var)
  | otherwise = IfaceTvBndr (toIfaceTvBndr var)

toIfaceBndrX :: VarSet -> Var -> IfaceBndr
toIfaceBndrX fr var
  | isId var  = IfaceIdBndr (toIfaceIdBndrX fr var)
  | otherwise = IfaceTvBndr (toIfaceTvBndrX fr var)

toIfaceForAllBndrs :: [VarBndr TyCoVar vis] -> [VarBndr IfaceBndr vis]
toIfaceForAllBndrs = map toIfaceForAllBndr

toIfaceForAllBndr :: VarBndr TyCoVar flag -> VarBndr IfaceBndr flag
toIfaceForAllBndr = toIfaceForAllBndrX emptyVarSet

toIfaceForAllBndrX :: VarSet -> (VarBndr TyCoVar flag) -> (VarBndr IfaceBndr flag)
toIfaceForAllBndrX fr (Bndr v vis) = Bndr (toIfaceBndrX fr v) vis

{-
************************************************************************
*                                                                      *
        Conversion from Type to IfaceType
*                                                                      *
************************************************************************
-}

toIfaceKind :: Type -> IfaceType
toIfaceKind = toIfaceType

---------------------
toIfaceType :: Type -> IfaceType
toIfaceType = toIfaceTypeX emptyVarSet

toIfaceTypeX :: VarSet -> Type -> IfaceType
-- (toIfaceTypeX free ty)
--    translates the tyvars in 'free' as IfaceFreeTyVars
--
-- Synonyms are retained in the interface type
toIfaceTypeX fr (TyVarTy tv)   -- See Note [Free TyVars and CoVars in IfaceType] in GHC.Iface.Type
  | tv `elemVarSet` fr         = IfaceFreeTyVar tv
  | otherwise                  = IfaceTyVar (toIfaceTyVar tv)
toIfaceTypeX fr ty@(AppTy {})  =
  -- Flatten as many argument AppTys as possible, then turn them into an
  -- IfaceAppArgs list.
  -- See Note [Suppressing invisible arguments] in GHC.Iface.Type.
  let (head, args) = splitAppTys ty
  in IfaceAppTy (toIfaceTypeX fr head) (toIfaceAppTyArgsX fr head args)
toIfaceTypeX _  (LitTy n)      = IfaceLitTy (toIfaceTyLit n)
toIfaceTypeX fr (ForAllTy b t) = IfaceForAllTy (toIfaceForAllBndrX fr b)
                                               (toIfaceTypeX (fr `delVarSet` binderVar b) t)
toIfaceTypeX fr (FunTy { ft_arg = t1, ft_mult = w, ft_res = t2, ft_af = af })
  = IfaceFunTy af (toIfaceTypeX fr w) (toIfaceTypeX fr t1) (toIfaceTypeX fr t2)
toIfaceTypeX fr (CastTy ty co)  = IfaceCastTy (toIfaceTypeX fr ty) (toIfaceCoercionX fr co)
toIfaceTypeX fr (CoercionTy co) = IfaceCoercionTy (toIfaceCoercionX fr co)

toIfaceTypeX fr (TyConApp tc tys)
    -- tuples
  | Just sort <- tyConTuple_maybe tc
  , n_tys == arity
  = IfaceTupleTy sort NotPromoted (toIfaceTcArgsX fr tc tys)

  | Just dc <- isPromotedDataCon_maybe tc
  , isBoxedTupleDataCon dc
  , n_tys == 2*arity
  = IfaceTupleTy BoxedTuple IsPromoted (toIfaceTcArgsX fr tc (drop arity tys))

  | tc `elem` [ eqPrimTyCon, eqReprPrimTyCon, heqTyCon ]
  , (k1:k2:_) <- tys
  = let info = mkIfaceTyConInfo NotPromoted sort
        sort | k1 `eqType` k2 = IfaceEqualityTyCon
             | otherwise      = IfaceNormalTyCon
    in IfaceTyConApp (IfaceTyCon (tyConName tc) info) (toIfaceTcArgsX fr tc tys)

    -- other applications
  | otherwise
  = IfaceTyConApp (toIfaceTyCon tc) (toIfaceTcArgsX fr tc tys)
  where
    arity = tyConArity tc
    n_tys = length tys

toIfaceTyVar :: TyVar -> IfLclName
toIfaceTyVar = mkIfLclName . occNameFS . getOccName

toIfaceCoVar :: CoVar -> IfLclName
toIfaceCoVar = mkIfLclName . occNameFS . getOccName

----------------
toIfaceTyCon :: TyCon -> IfaceTyCon
toIfaceTyCon tc
  = IfaceTyCon tc_name info
  where
    tc_name = tyConName tc
    info    = mkIfaceTyConInfo promoted sort
    promoted | isDataKindsPromotedDataCon tc = IsPromoted
             | otherwise            = NotPromoted

    tupleSort :: TyCon -> Maybe IfaceTyConSort
    tupleSort tc' =
        case tyConTuple_maybe tc' of
          Just UnboxedTuple -> let arity = tyConArity tc' `div` 2
                               in Just $ IfaceTupleTyCon arity UnboxedTuple
          Just sort         -> let arity = tyConArity tc'
                               in Just $ IfaceTupleTyCon arity sort
          Nothing           -> Nothing

    sort
      | Just tsort <- tupleSort tc           = tsort

      | Just dcon <- isPromotedDataCon_maybe tc
      , let tc' = dataConTyCon dcon
      , Just tsort <- tupleSort tc'          = tsort

      | isUnboxedSumTyCon tc
      , Just cons <- tyConDataCons_maybe tc  = IfaceSumTyCon (length cons)

      | otherwise                            = IfaceNormalTyCon


toIfaceTyCon_name :: Name -> IfaceTyCon
toIfaceTyCon_name n = IfaceTyCon n info
  where info = mkIfaceTyConInfo NotPromoted IfaceNormalTyCon
  -- Used for the "rough-match" tycon stuff,
  -- where pretty-printing is not an issue

toIfaceTyLit :: TyLit -> IfaceTyLit
toIfaceTyLit (NumTyLit x) = IfaceNumTyLit x
toIfaceTyLit (StrTyLit x) = IfaceStrTyLit (LexicalFastString x)
toIfaceTyLit (CharTyLit x) = IfaceCharTyLit x

----------------
toIfaceCoercion :: Coercion -> IfaceCoercion
toIfaceCoercion = toIfaceCoercionX emptyVarSet

toIfaceCoercionX :: VarSet -> Coercion -> IfaceCoercion
-- (toIfaceCoercionX free ty)
--    translates the tyvars in 'free' as IfaceFreeTyVars
toIfaceCoercionX fr co
  = go co
  where
    go_mco MRefl     = IfaceMRefl
    go_mco (MCo co)  = IfaceMCo $ go co

    go (Refl ty)            = IfaceReflCo (toIfaceTypeX fr ty)
    go (GRefl r ty mco)     = IfaceGReflCo r (toIfaceTypeX fr ty) (go_mco mco)
    go (CoVarCo cv)
      -- See Note [Free TyVars and CoVars in IfaceType] in GHC.Iface.Type
      | cv `elemVarSet` fr = IfaceFreeCoVar cv
      | otherwise          = IfaceCoVarCo (toIfaceCoVar cv)
    go (HoleCo h)          = IfaceHoleCo  (coHoleCoVar h)

    go (AppCo co1 co2)     = IfaceAppCo  (go co1) (go co2)
    go (SymCo co)          = IfaceSymCo (go co)
    go (TransCo co1 co2)   = IfaceTransCo (go co1) (go co2)
    go (SelCo d co)        = IfaceSelCo d (go co)
    go (LRCo lr co)        = IfaceLRCo lr (go co)
    go (InstCo co arg)     = IfaceInstCo (go co) (go arg)
    go (KindCo c)          = IfaceKindCo (go c)
    go (SubCo co)          = IfaceSubCo (go co)
    go (AxiomCo ax cs)     = IfaceAxiomCo (toIfaceAxiomRule ax) (map go cs)
    go (UnivCo { uco_prov = p, uco_role = r, uco_lty = t1, uco_rty = t2, uco_deps = deps })
        = IfaceUnivCo p r (toIfaceTypeX fr t1) (toIfaceTypeX fr t2) (map go deps)

    go co@(TyConAppCo r tc cos)
      =  assertPpr (isNothing (tyConAppFunCo_maybe r tc cos)) (ppr co) $
         IfaceTyConAppCo r (toIfaceTyCon tc) (map go cos)

    go (FunCo { fco_role = r, fco_mult = w, fco_arg = co1, fco_res = co2 })
      = IfaceFunCo r (go w) (go co1) (go co2)

    go (ForAllCo tv visL visR k co)
      = IfaceForAllCo (toIfaceBndr tv)
                      visL
                      visR
                      (toIfaceCoercionX fr' k)
                      (toIfaceCoercionX fr' co)
                          where
                            fr' = fr `delVarSet` tv

toIfaceAxiomRule :: CoAxiomRule -> IfaceAxiomRule
toIfaceAxiomRule (BuiltInFamRew  bif) = IfaceAR_X (mkIfLclName (bifrw_name bif))
toIfaceAxiomRule (BuiltInFamInj bif)  = IfaceAR_X (mkIfLclName (bifinj_name bif))
toIfaceAxiomRule (BranchedAxiom ax i) = IfaceAR_B (coAxiomName ax) i
toIfaceAxiomRule (UnbranchedAxiom ax) = IfaceAR_U (coAxiomName ax)

toIfaceTcArgs :: TyCon -> [Type] -> IfaceAppArgs
toIfaceTcArgs = toIfaceTcArgsX emptyVarSet

toIfaceTcArgsX :: VarSet -> TyCon -> [Type] -> IfaceAppArgs
toIfaceTcArgsX fr tc ty_args = toIfaceAppArgsX fr (tyConKind tc) ty_args

toIfaceAppTyArgsX :: VarSet -> Type -> [Type] -> IfaceAppArgs
toIfaceAppTyArgsX fr ty ty_args = toIfaceAppArgsX fr (typeKind ty) ty_args

toIfaceAppArgsX :: VarSet -> Kind -> [Type] -> IfaceAppArgs
-- See Note [Suppressing invisible arguments] in GHC.Iface.Type
-- We produce a result list of args describing visibility
-- The awkward case is
--    T :: forall k. * -> k
-- And consider
--    T (forall j. blah) * blib
-- Is 'blib' visible?  It depends on the visibility flag on j,
-- so we have to substitute for k.  Annoying!
toIfaceAppArgsX fr kind ty_args
  | null ty_args
  = IA_Nil
  | otherwise
  = go (mkEmptySubst in_scope) kind ty_args
  where
    in_scope = mkInScopeSet (tyCoVarsOfTypes ty_args)

    go _   _                   []     = IA_Nil
    go env ty                  ts
      | Just ty' <- coreView ty
      = go env ty' ts
    go env (ForAllTy (Bndr tv vis) res) (t:ts)
      = IA_Arg t' vis ts'
      where
        t'  = toIfaceTypeX fr t
        ts' = go (extendTCvSubst env tv t) res ts

    go env (FunTy { ft_af = af, ft_res = res }) (t:ts)
      = assert (isVisibleFunArg af)
        IA_Arg (toIfaceTypeX fr t) Required (go env res ts)

    go env ty ts@(t1:ts1)
      | not (isEmptyTCvSubst env)
      = go (zapSubst env) (substTy env ty) ts
        -- See Note [Care with kind instantiation] in GHC.Core.Type

      | otherwise
      = -- There's a kind error in the type we are trying to print
        -- e.g. kind = k, ty_args = [Int]
        -- This is probably a compiler bug, so we print a trace and
        -- carry on as if it were FunTy.  Without the test for
        -- isEmptyTCvSubst we'd get an infinite loop (#15473)
        warnPprTrace True "toIfaceAppArgsX" (ppr kind $$ ppr ty_args) $
        IA_Arg (toIfaceTypeX fr t1) Required (go env ty ts1)

tidyToIfaceType :: TidyEnv -> Type -> IfaceType
tidyToIfaceType env ty = toIfaceType (tidyType env ty)

tidyToIfaceTcArgs :: TidyEnv -> TyCon -> [Type] -> IfaceAppArgs
tidyToIfaceTcArgs env tc tys = toIfaceTcArgs tc (tidyTypes env tys)

tidyToIfaceContext :: TidyEnv -> ThetaType -> IfaceContext
tidyToIfaceContext env theta = map (tidyToIfaceType env) theta

{-
************************************************************************
*                                                                      *
        Conversion of pattern synonyms
*                                                                      *
************************************************************************
-}

patSynToIfaceDecl :: PatSyn -> IfaceDecl
patSynToIfaceDecl ps
  = IfacePatSyn { ifName          = getName $ ps
                , ifPatMatcher    = to_if_pr (patSynMatcher ps)
                , ifPatBuilder    = fmap to_if_pr (patSynBuilder ps)
                , ifPatIsInfix    = patSynIsInfix ps
                , ifPatUnivBndrs  = map toIfaceForAllBndr univ_bndrs'
                , ifPatExBndrs    = map toIfaceForAllBndr ex_bndrs'
                , ifPatProvCtxt   = tidyToIfaceContext env2 prov_theta
                , ifPatReqCtxt    = tidyToIfaceContext env2 req_theta
                , ifPatArgs       = map (tidyToIfaceType env2 . scaledThing) args
                , ifPatTy         = tidyToIfaceType env2 rhs_ty
                , ifFieldLabels   = (patSynFieldLabels ps)
                }
  where
    (_univ_tvs, req_theta, _ex_tvs, prov_theta, args, rhs_ty) = patSynSig ps
    univ_bndrs = patSynUnivTyVarBinders ps
    ex_bndrs   = patSynExTyVarBinders ps
    (env1, univ_bndrs') = tidyForAllTyBinders emptyTidyEnv univ_bndrs
    (env2, ex_bndrs')   = tidyForAllTyBinders env1 ex_bndrs
    to_if_pr (name, _type, needs_dummy) = (name, needs_dummy)

{-
************************************************************************
*                                                                      *
        Conversion of other things
*                                                                      *
************************************************************************
-}

toIfaceBang :: TidyEnv -> HsImplBang -> IfaceBang
toIfaceBang _    HsLazy              = IfNoBang
toIfaceBang _   (HsUnpack Nothing)   = IfUnpack
toIfaceBang env (HsUnpack (Just co)) = IfUnpackCo (toIfaceCoercion (tidyCo env co))
toIfaceBang _   (HsStrict _)         = IfStrict

toIfaceSrcBang :: HsSrcBang -> IfaceSrcBang
toIfaceSrcBang (HsSrcBang _ unpk bang) = IfSrcBang unpk bang

toIfaceLetBndr :: Id -> IfaceLetBndr
toIfaceLetBndr id  = IfLetBndr (mkIfLclName (occNameFS (getOccName id)))
                               (toIfaceType (idType id))
                               (toIfaceIdInfo (idInfo id))
                               (idJoinPointHood id)
  -- Put into the interface file any IdInfo that GHC.Core.Tidy.tidyLetBndr
  -- has left on the Id.  See Note [IdInfo on nested let-bindings] in GHC.Iface.Syntax

toIfaceTopBndr :: Id -> IfaceTopBndrInfo
toIfaceTopBndr id
  = if isExternalName name
      then IfGblTopBndr name
      else IfLclTopBndr (mkIfLclName (occNameFS (getOccName id))) (toIfaceType (idType id))
                        (toIfaceIdInfo (idInfo id)) (toIfaceIdDetails (idDetails id))
  where
    name = getName id

toIfaceIdDetails :: IdDetails -> IfaceIdDetails
toIfaceIdDetails VanillaId                      = IfVanillaId
toIfaceIdDetails (WorkerLikeId dmds)            = IfWorkerLikeId dmds
toIfaceIdDetails (DFunId {})                    = IfDFunId
toIfaceIdDetails (RecSelId { sel_naughty = n
                           , sel_tycon = tc
                           , sel_fieldLabel = fl }) =
  let (iface, first_con) = case tc of
                RecSelData ty_con    -> ( Left (toIfaceTyCon ty_con), dataConName $ head $ tyConDataCons ty_con)
                RecSelPatSyn pat_syn -> ( Right (patSynToIfaceDecl pat_syn), patSynName pat_syn)
  in IfRecSelId iface first_con n fl

  -- The remaining cases are all "implicit Ids" which don't
  -- appear in interface files at all
toIfaceIdDetails other = pprTrace "toIfaceIdDetails" (ppr other)
                         IfVanillaId   -- Unexpected; the other

toIfaceIdInfo :: IdInfo -> IfaceIdInfo
toIfaceIdInfo id_info
  = catMaybes [arity_hsinfo, caf_hsinfo, strict_hsinfo, cpr_hsinfo,
               inline_hsinfo,  unfold_hsinfo]
               -- NB: strictness and arity must appear in the list before unfolding
               -- See GHC.IfaceToCore.tcUnfolding
  where
    ------------  Arity  --------------
    arity_info = arityInfo id_info
    arity_hsinfo | arity_info == 0 = Nothing
                 | otherwise       = Just (HsArity arity_info)

    ------------ Caf Info --------------
    caf_info   = cafInfo id_info
    caf_hsinfo = case caf_info of
                   NoCafRefs -> Just HsNoCafRefs
                   _other    -> Nothing

    ------------  Strictness  --------------
        -- No point in explicitly exporting TopSig
    sig_info = dmdSigInfo id_info
    strict_hsinfo | not (isNopSig sig_info) = Just (HsDmdSig sig_info)
                  | otherwise               = Nothing

    ------------  CPR --------------
    cpr_info = cprSigInfo id_info
    cpr_hsinfo | cpr_info /= topCprSig = Just (HsCprSig cpr_info)
               | otherwise             = Nothing
    ------------  Unfolding  --------------
    unfold_hsinfo = toIfUnfolding loop_breaker (realUnfoldingInfo id_info)
    loop_breaker  = isStrongLoopBreaker (occInfo id_info)

    ------------  Inline prag  --------------
    inline_prag = inlinePragInfo id_info
    inline_hsinfo | isDefaultInlinePragma inline_prag = Nothing
                  | otherwise = Just (HsInline inline_prag)

--------------------------
toIfUnfolding :: Bool -> Unfolding -> Maybe IfaceInfoItem
toIfUnfolding lb (CoreUnfolding { uf_tmpl = rhs
                                , uf_src = src
                                , uf_cache = cache
                                , uf_guidance = guidance })
  = Just $ HsUnfold lb $
    IfCoreUnfold src cache (toIfGuidance src guidance) (toIfaceExpr rhs)
        -- Yes, even if guidance is UnfNever, expose the unfolding
        -- If we didn't want to expose the unfolding, GHC.Iface.Tidy would
        -- have stuck in NoUnfolding.  For supercompilation we want
        -- to see that unfolding!

toIfUnfolding lb (DFunUnfolding { df_bndrs = bndrs, df_args = args })
  = Just (HsUnfold lb (IfDFunUnfold (map toIfaceBndr bndrs) (map toIfaceExpr args)))
      -- No need to serialise the data constructor;
      -- we can recover it from the type of the dfun

toIfUnfolding _ (OtherCon {}) = Nothing
  -- The binding site of an Id doesn't have OtherCon, except perhaps
  -- where we have called trimUnfolding; and that evald'ness info is
  -- not needed by importing modules

toIfUnfolding _ BootUnfolding = Nothing
  -- Can't happen; we only have BootUnfolding for imported binders

toIfUnfolding _ NoUnfolding = Nothing

toIfGuidance :: UnfoldingSource -> UnfoldingGuidance -> IfGuidance
toIfGuidance src guidance
  | UnfWhen arity unsat_ok boring_ok <- guidance
  , isStableSource src = IfWhen arity unsat_ok boring_ok
  | otherwise          = IfNoGuidance

toIfaceBooleanFormula :: BF.BooleanFormula GhcRn -> IfaceBooleanFormula
toIfaceBooleanFormula = go
  where
    go (BF.Var nm   ) = IfVar    $ mkIfLclName . getOccFS . unLoc $  nm
    go (BF.And bfs  ) = IfAnd    $ map (go . unLoc) bfs
    go (BF.Or bfs   ) = IfOr     $ map (go . unLoc) bfs
    go (BF.Parens bf) = IfParens $     (go . unLoc) bf

{-
************************************************************************
*                                                                      *
        Conversion of expressions
*                                                                      *
************************************************************************
-}

toIfaceExpr :: CoreExpr -> IfaceExpr
toIfaceExpr (Var v)         = toIfaceVar v
toIfaceExpr (Lit (LitRubbish tc r)) = IfaceLitRubbish tc (toIfaceType r)
toIfaceExpr (Lit l)         = IfaceLit l
toIfaceExpr (Type ty)       = IfaceType (toIfaceType ty)
toIfaceExpr (Coercion co)   = IfaceCo   (toIfaceCoercion co)
toIfaceExpr (Lam x b)       = IfaceLam (toIfaceBndr x, toIfaceOneShot x) (toIfaceExpr b)
toIfaceExpr (App f a)       = toIfaceApp f [a]
toIfaceExpr (Case s x ty as)
  | null as                 = IfaceECase (toIfaceExpr s) (toIfaceType ty)
  | otherwise               = IfaceCase (toIfaceExpr s) (mkIfLclName (getOccFS x)) (map toIfaceAlt as)
toIfaceExpr (Let b e)       = IfaceLet (toIfaceBind b) (toIfaceExpr e)
toIfaceExpr (Cast e co)     = IfaceCast (toIfaceExpr e) (toIfaceCoercion co)
toIfaceExpr (Tick t e)      = IfaceTick (toIfaceTickish t) (toIfaceExpr e)

toIfaceOneShot :: Id -> IfaceOneShot
toIfaceOneShot id | isId id
                  , OneShotLam <- oneShotInfo (idInfo id)
                  = IfaceOneShot
                  | otherwise
                  = IfaceNoOneShot

---------------------
toIfaceTickish :: CoreTickish -> IfaceTickish
toIfaceTickish (ProfNote cc tick push) = IfaceSCC cc tick push
toIfaceTickish (HpcTick modl ix)       = IfaceHpcTick modl ix
toIfaceTickish (SourceNote src (LexicalFastString names)) =
  IfaceSource src names
toIfaceTickish (Breakpoint _ ix fv) =
  IfaceBreakpoint ix (toIfaceVar <$> fv)

---------------------
toIfaceBind :: Bind Id -> IfaceBinding IfaceLetBndr
toIfaceBind (NonRec b r) = IfaceNonRec (toIfaceLetBndr b) (toIfaceExpr r)
toIfaceBind (Rec prs)    = IfaceRec [(toIfaceLetBndr b, toIfaceExpr r) | (b,r) <- prs]

toIfaceTopBind :: Bind Id -> IfaceBindingX IfaceMaybeRhs IfaceTopBndrInfo
toIfaceTopBind b =
  case b of
    NonRec b r -> uncurry IfaceNonRec (do_one (b, r))
    Rec prs -> IfaceRec (map do_one prs)
  where
        do_one (b, rhs) =
          let top_bndr = toIfaceTopBndr b
              rhs' = case top_bndr of
                      -- Use the existing unfolding for a global binder if we store that anyway.
                      -- See Note [Interface File with Core: Sharing RHSs]
                      IfGblTopBndr {} -> if already_has_unfolding b then IfUseUnfoldingRhs else IfRhs (toIfaceExpr rhs)
                      -- Local binders will have had unfoldings trimmed so have
                      -- to serialise the whole RHS.
                      IfLclTopBndr {} -> IfRhs (toIfaceExpr rhs)
          in (top_bndr, rhs')

        -- The sharing behaviour is currently disabled due to #22807, and relies on
        -- finished #20056 to be re-enabled.
        disabledDueTo22807 = True

        already_has_unfolding b = not disabledDueTo22807
                                && -- The identifier has an unfolding, which we are going to serialise anyway
                                hasCoreUnfolding (realIdUnfolding b)
                                -- But not a stable unfolding, we want the optimised unfoldings.
                                && not (isStableUnfolding (realIdUnfolding b))

---------------------
toIfaceAlt :: CoreAlt -> IfaceAlt
toIfaceAlt (Alt c bs r) = IfaceAlt (toIfaceCon c) (map (mkIfLclName . getOccFS) bs) (toIfaceExpr r)

---------------------
toIfaceCon :: AltCon -> IfaceConAlt
toIfaceCon (DataAlt dc) = IfaceDataAlt (getName dc)
toIfaceCon (LitAlt l)   = assertPpr (not (isLitRubbish l)) (ppr l) $
                          -- assert: see Note [Rubbish literals] wrinkle (b)
                          IfaceLitAlt l
toIfaceCon DEFAULT      = IfaceDefaultAlt

---------------------
toIfaceApp :: Expr CoreBndr -> [Arg CoreBndr] -> IfaceExpr
toIfaceApp (App f a) as = toIfaceApp f (a:as)
toIfaceApp (Var v) as
  = case isDataConWorkId_maybe v of
        -- We convert the *worker* for tuples into IfaceTuples
        Just dc |  saturated
                ,  Just tup_sort <- tyConTuple_maybe tc
                -> IfaceTuple tup_sort tup_args
          where
            val_args  = dropWhile isTypeArg as
            saturated = val_args `lengthIs` idArity v
            tup_args  = map toIfaceExpr val_args
            tc        = dataConTyCon dc

        _ -> mkIfaceApps (toIfaceVar v) as

toIfaceApp e as = mkIfaceApps (toIfaceExpr e) as

mkIfaceApps :: IfaceExpr -> [CoreExpr] -> IfaceExpr
mkIfaceApps f as = foldl' (\f a -> IfaceApp f (toIfaceExpr a)) f as

---------------------
toIfaceVar :: Id -> IfaceExpr
toIfaceVar v
    | isBootUnfolding (idUnfolding v)
    = -- See Note [Inlining and hs-boot files]
      IfaceApp (IfaceApp (IfaceExt noinline_id)
                         (IfaceType (toIfaceType ty)))
               (IfaceExt name) -- don't use mkIfaceApps, or infinite loop

    | Just fcall <- isFCallId_maybe v = IfaceFCall fcall (toIfaceType (idType v))
                                      -- Foreign calls have special syntax

    | isExternalName name             = IfaceExt name
    | otherwise                       = IfaceLcl (mkIfLclName (occNameFS $ nameOccName name))
  where
    name = idName v
    ty   = idType v
    noinline_id | isConstraintKind (typeKind ty) = noinlineConstraintIdName
                | otherwise                      = noinlineIdName



---------------------
toIfaceLFInfo :: Name -> LambdaFormInfo -> IfaceLFInfo
toIfaceLFInfo nm lfi = case lfi of
    LFReEntrant top_lvl arity no_fvs _arg_descr ->
      -- Exported LFReEntrant closures are top level, and top-level closures
      -- don't have free variables
      assertPpr (isTopLevel top_lvl) (ppr nm) $
      assertPpr no_fvs (ppr nm) $
      IfLFReEntrant arity
    LFThunk top_lvl no_fvs updatable sfi mb_fun ->
      -- Exported LFThunk closures are top level (which don't have free
      -- variables) and non-standard (see cgTopRhsClosure)
      assertPpr (isTopLevel top_lvl) (ppr nm) $
      assertPpr no_fvs (ppr nm) $
      assertPpr (sfi == NonStandardThunk) (ppr nm) $
      IfLFThunk updatable mb_fun
    LFCon dc ->
      IfLFCon (dataConName dc)
    LFUnknown mb_fun ->
      IfLFUnknown mb_fun
    LFUnlifted ->
      IfLFUnlifted
    LFLetNoEscape ->
      panic "toIfaceLFInfo: LFLetNoEscape"

-- Dehydrating CgBreakInfo

dehydrateCgBreakInfo :: [TyVar] -> [Maybe (Id, Word)] -> Type -> BreakpointId -> CgBreakInfo
dehydrateCgBreakInfo ty_vars idOffSets tick_ty bid =
          CgBreakInfo
            { cgb_tyvars = map toIfaceTvBndr ty_vars
            , cgb_vars = map (fmap (\(i, offset) -> (toIfaceIdBndr i, offset))) idOffSets
            , cgb_resty = toIfaceType tick_ty
            , cgb_tick_id = bid
            }

{- Note [Inlining and hs-boot files]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this example (#10083, #12789):

    ---------- RSR.hs-boot ------------
    module RSR where
      data RSR
      eqRSR :: RSR -> RSR -> Bool

    ---------- SR.hs ------------
    module SR where
      import {-# SOURCE #-} RSR
      data SR = MkSR RSR
      eqSR (MkSR r1) (MkSR r2) = eqRSR r1 r2

    ---------- RSR.hs ------------
    module RSR where
      import SR
      data RSR = MkRSR SR -- deriving( Eq )
      eqRSR (MkRSR s1) (MkRSR s2) = (eqSR s1 s2)
      foo x y = not (eqRSR x y)

When compiling RSR we get this code

    RSR.eqRSR :: RSR -> RSR -> Bool
    RSR.eqRSR = \ (ds1 :: RSR.RSR) (ds2 :: RSR.RSR) ->
                case ds1 of _ { RSR.MkRSR s1 ->
                case ds2 of _ { RSR.MkRSR s2 ->
                SR.eqSR s1 s2 }}

    RSR.foo :: RSR -> RSR -> Bool
    RSR.foo = \ (x :: RSR) (y :: RSR) -> not (RSR.eqRSR x y)

Now, when optimising foo:
    Inline eqRSR (small, non-rec)
    Inline eqSR  (small, non-rec)
but the result of inlining eqSR from SR is another call to eqRSR, so
everything repeats.  Neither eqSR nor eqRSR are (apparently) loop
breakers.

Solution: in the unfolding of eqSR in SR.hi, replace `eqRSR` in SR
with `noinline eqRSR`, so that eqRSR doesn't get inlined.  This means
that when GHC inlines `eqSR`, it will not also inline `eqRSR`, exactly
as would have been the case if `foo` had been defined in SR.hs (and
marked as a loop-breaker).

But how do we arrange for this to happen?  There are two ingredients:

    1. When we serialize out unfoldings to IfaceExprs (toIfaceVar),
    for every variable reference we see if we are referring to an
    'Id' that came from an hs-boot file.  If so, we add a `noinline`
    to the reference.  See Note [noinlineId magic]
    in GHC.Types.Id.Make

    2. But how do we know if a reference came from an hs-boot file
    or not?  We could record this directly in the 'IdInfo', but
    actually we deduce this by looking at the unfolding: 'Id's
    that come from boot files are given a special unfolding
    (upon typechecking) 'BootUnfolding' which say that there is
    no unfolding, and the reason is because the 'Id' came from
    a boot file.

Here is a solution that doesn't work: when compiling RSR,
add a NOINLINE pragma to every function exported by the boot-file
for RSR (if it exists).  Doing so makes the bootstrapped GHC itself
slower by 8% overall (on #9872a-d, and T1969: the reason
is that these NOINLINE'd functions now can't be profitably inlined
outside of the hs-boot loop.

Note [Interface File with Core: Sharing RHSs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

IMPORTANT: This optimisation is currently disabled due to #22807, it can be
           re-enabled once #22056 is implemented.

In order to avoid duplicating definitions for bindings which already have unfoldings
we do some minor headstands to avoid serialising the RHS of a definition if it has
*any* unfolding.

* Only global things have unfoldings, because local things have had their unfoldings stripped.
* For any global thing which has an unstable unfolding, we just use that.

In order to implement this sharing:

* When creating the interface, check the criteria above and don't serialise the RHS
  if such a case.

* When reading an interface, look at the realIdUnfolding, and then the
  maybeUnfoldingTemplate.  See `tc_iface_binding` for where this happens.

There are two main reasons why the mi_extra_decls field exists rather than shoe-horning
all the core bindings

1. mi_extra_decls retains the recursive group structure of the original program which
   is very convenient as otherwise we would have to do the analysis again when loading
   the program.
2. There are additional local top-level bindings which don't make it into mi_decls. It's
   best to keep these separate from mi_decls as mi_decls is used to compute the ABI hash.

-}
