{-# LANGUAGE CPP #-}
{-# LANGUAGE Strict #-} -- See Note [Avoiding space leaks in toIface*]

-- | Functions for converting Core things to interface file things.
module GHC.CoreToIface
    ( -- * Binders
      toIfaceTvBndr
    , toIfaceTvBndrs
    , toIfaceIdBndr
    , toIfaceBndr
    , toIfaceForAllBndr
    , toIfaceTyCoVarBinders
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
    , toIfaceAlt
    , toIfaceCon
    , toIfaceApp
    , toIfaceVar
      -- * Other stuff
    , toIfaceLFInfo
    ) where

#include "HsVersions.h"

import GhcPrelude

import GHC.Iface.Syntax
import GHC.Core.DataCon
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.StgToCmm.Types
import GHC.Core
import GHC.Core.TyCon hiding ( pprPromotionQuote )
import GHC.Core.Coercion.Axiom
import TysPrim ( eqPrimTyCon, eqReprPrimTyCon )
import TysWiredIn ( heqTyCon )
import GHC.Types.Id.Make ( noinlineIdName )
import PrelNames
import GHC.Types.Name
import GHC.Types.Basic
import GHC.Core.Type
import GHC.Core.PatSyn
import Outputable
import FastString
import Util
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Tidy ( tidyCo )
import GHC.Types.Demand ( isTopSig )
import GHC.Types.Cpr ( topCprSig )

import Data.Maybe ( catMaybes )
import Data.Word
import Data.Bits

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
toIfaceTvBndrX fr tyvar = ( occNameFS (getOccName tyvar)
                          , toIfaceTypeX fr (tyVarKind tyvar)
                          )

toIfaceTvBndrs :: [TyVar] -> [IfaceTvBndr]
toIfaceTvBndrs = map toIfaceTvBndr

toIfaceIdBndr :: Id -> IfaceIdBndr
toIfaceIdBndr = toIfaceIdBndrX emptyVarSet

toIfaceIdBndrX :: VarSet -> CoVar -> IfaceIdBndr
toIfaceIdBndrX fr covar = ( occNameFS (getOccName covar)
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

toIfaceTyCoVarBinder :: VarBndr Var vis -> VarBndr IfaceBndr vis
toIfaceTyCoVarBinder (Bndr tv vis) = Bndr (toIfaceBndr tv) vis

toIfaceTyCoVarBinders :: [VarBndr Var vis] -> [VarBndr IfaceBndr vis]
toIfaceTyCoVarBinders = map toIfaceTyCoVarBinder

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
toIfaceTypeX fr (TyVarTy tv)   -- See Note [TcTyVars in IfaceType] in GHC.Iface.Type
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
toIfaceTypeX fr (FunTy { ft_arg = t1, ft_res = t2, ft_af = af })
  = IfaceFunTy af (toIfaceTypeX fr t1) (toIfaceTypeX fr t2)
toIfaceTypeX fr (CastTy ty co)  = IfaceCastTy (toIfaceTypeX fr ty) (toIfaceCoercionX fr co)
toIfaceTypeX fr (CoercionTy co) = IfaceCoercionTy (toIfaceCoercionX fr co)

toIfaceTypeX fr (TyConApp tc tys)
    -- tuples
  | Just sort <- tyConTuple_maybe tc
  , n_tys == arity
  = IfaceTupleTy sort NotPromoted (toIfaceTcArgsX fr tc tys)

  | Just dc <- isPromotedDataCon_maybe tc
  , isTupleDataCon dc
  , n_tys == 2*arity
  = IfaceTupleTy BoxedTuple IsPromoted (toIfaceTcArgsX fr tc (drop arity tys))

  | tc `elem` [ eqPrimTyCon, eqReprPrimTyCon, heqTyCon ]
  , (k1:k2:_) <- tys
  = let info = IfaceTyConInfo NotPromoted sort
        sort | k1 `eqType` k2 = IfaceEqualityTyCon
             | otherwise      = IfaceNormalTyCon
    in IfaceTyConApp (IfaceTyCon (tyConName tc) info) (toIfaceTcArgsX fr tc tys)

    -- other applications
  | otherwise
  = IfaceTyConApp (toIfaceTyCon tc) (toIfaceTcArgsX fr tc tys)
  where
    arity = tyConArity tc
    n_tys = length tys

toIfaceTyVar :: TyVar -> FastString
toIfaceTyVar = occNameFS . getOccName

toIfaceCoVar :: CoVar -> FastString
toIfaceCoVar = occNameFS . getOccName

toIfaceForAllBndr :: TyCoVarBinder -> IfaceForAllBndr
toIfaceForAllBndr = toIfaceForAllBndrX emptyVarSet

toIfaceForAllBndrX :: VarSet -> TyCoVarBinder -> IfaceForAllBndr
toIfaceForAllBndrX fr (Bndr v vis) = Bndr (toIfaceBndrX fr v) vis

----------------
toIfaceTyCon :: TyCon -> IfaceTyCon
toIfaceTyCon tc
  = IfaceTyCon tc_name info
  where
    tc_name = tyConName tc
    info    = IfaceTyConInfo promoted sort
    promoted | isPromotedDataCon tc = IsPromoted
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
      , Just cons <- isDataSumTyCon_maybe tc = IfaceSumTyCon (length cons)

      | otherwise                            = IfaceNormalTyCon


toIfaceTyCon_name :: Name -> IfaceTyCon
toIfaceTyCon_name n = IfaceTyCon n info
  where info = IfaceTyConInfo NotPromoted IfaceNormalTyCon
  -- Used for the "rough-match" tycon stuff,
  -- where pretty-printing is not an issue

toIfaceTyLit :: TyLit -> IfaceTyLit
toIfaceTyLit (NumTyLit x) = IfaceNumTyLit x
toIfaceTyLit (StrTyLit x) = IfaceStrTyLit x

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
      -- See [TcTyVars in IfaceType] in GHC.Iface.Type
      | cv `elemVarSet` fr  = IfaceFreeCoVar cv
      | otherwise           = IfaceCoVarCo (toIfaceCoVar cv)
    go (HoleCo h)           = IfaceHoleCo  (coHoleCoVar h)

    go (AppCo co1 co2)      = IfaceAppCo  (go co1) (go co2)
    go (SymCo co)           = IfaceSymCo (go co)
    go (TransCo co1 co2)    = IfaceTransCo (go co1) (go co2)
    go (NthCo _r d co)      = IfaceNthCo d (go co)
    go (LRCo lr co)         = IfaceLRCo lr (go co)
    go (InstCo co arg)      = IfaceInstCo (go co) (go arg)
    go (KindCo c)           = IfaceKindCo (go c)
    go (SubCo co)           = IfaceSubCo (go co)
    go (AxiomRuleCo co cs)  = IfaceAxiomRuleCo (coaxrName co) (map go cs)
    go (AxiomInstCo c i cs) = IfaceAxiomInstCo (coAxiomName c) i (map go cs)
    go (UnivCo p r t1 t2)   = IfaceUnivCo (go_prov p) r
                                          (toIfaceTypeX fr t1)
                                          (toIfaceTypeX fr t2)
    go (TyConAppCo r tc cos)
      | tc `hasKey` funTyConKey
      , [_,_,_,_] <- cos         = pprPanic "toIfaceCoercion" (ppr co)
      | otherwise                = IfaceTyConAppCo r (toIfaceTyCon tc) (map go cos)
    go (FunCo r co1 co2)   = IfaceFunCo r (go co1) (go co2)

    go (ForAllCo tv k co) = IfaceForAllCo (toIfaceBndr tv)
                                          (toIfaceCoercionX fr' k)
                                          (toIfaceCoercionX fr' co)
                          where
                            fr' = fr `delVarSet` tv

    go_prov :: UnivCoProvenance -> IfaceUnivCoProv
    go_prov (PhantomProv co)    = IfacePhantomProv (go co)
    go_prov (ProofIrrelProv co) = IfaceProofIrrelProv (go co)
    go_prov (PluginProv str)    = IfacePluginProv str

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
  = go (mkEmptyTCvSubst in_scope) kind ty_args
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
      = IA_Arg (toIfaceTypeX fr t) argf (go env res ts)
      where
        argf = case af of
                 VisArg   -> Required
                 InvisArg -> Inferred
                   -- It's rare for a kind to have a constraint argument, but
                   -- it can happen. See Note [AnonTCB InvisArg] in GHC.Core.TyCon.

    go env ty ts@(t1:ts1)
      | not (isEmptyTCvSubst env)
      = go (zapTCvSubst env) (substTy env ty) ts
        -- See Note [Care with kind instantiation] in GHC.Core.Type

      | otherwise
      = -- There's a kind error in the type we are trying to print
        -- e.g. kind = k, ty_args = [Int]
        -- This is probably a compiler bug, so we print a trace and
        -- carry on as if it were FunTy.  Without the test for
        -- isEmptyTCvSubst we'd get an infinite loop (#15473)
        WARN( True, ppr kind $$ ppr ty_args )
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
                , ifPatArgs       = map (tidyToIfaceType env2) args
                , ifPatTy         = tidyToIfaceType env2 rhs_ty
                , ifFieldLabels   = (patSynFieldLabels ps)
                }
  where
    (_univ_tvs, req_theta, _ex_tvs, prov_theta, args, rhs_ty) = patSynSig ps
    univ_bndrs = patSynUnivTyVarBinders ps
    ex_bndrs   = patSynExTyVarBinders ps
    (env1, univ_bndrs') = tidyTyCoVarBinders emptyTidyEnv univ_bndrs
    (env2, ex_bndrs')   = tidyTyCoVarBinders env1 ex_bndrs
    to_if_pr (id, needs_dummy) = (idName id, needs_dummy)

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
toIfaceBang _   HsStrict             = IfStrict

toIfaceSrcBang :: HsSrcBang -> IfaceSrcBang
toIfaceSrcBang (HsSrcBang _ unpk bang) = IfSrcBang unpk bang

toIfaceLetBndr :: Id -> IfaceLetBndr
toIfaceLetBndr id  = IfLetBndr (occNameFS (getOccName id))
                               (toIfaceType (idType id))
                               (toIfaceIdInfo (idInfo id))
                               (toIfaceJoinInfo (isJoinId_maybe id))
  -- Put into the interface file any IdInfo that GHC.Core.Op.Tidy.tidyLetBndr
  -- has left on the Id.  See Note [IdInfo on nested let-bindings] in GHC.Iface.Syntax

toIfaceIdDetails :: IdDetails -> IfaceIdDetails
toIfaceIdDetails VanillaId                      = IfVanillaId
toIfaceIdDetails (DFunId {})                    = IfDFunId
toIfaceIdDetails (RecSelId { sel_naughty = n
                           , sel_tycon = tc })  =
  let iface = case tc of
                RecSelData ty_con -> Left (toIfaceTyCon ty_con)
                RecSelPatSyn pat_syn -> Right (patSynToIfaceDecl pat_syn)
  in IfRecSelId iface n

  -- The remaining cases are all "implicit Ids" which don't
  -- appear in interface files at all
toIfaceIdDetails other = pprTrace "toIfaceIdDetails" (ppr other)
                         IfVanillaId   -- Unexpected; the other

toIfaceIdInfo :: IdInfo -> IfaceIdInfo
toIfaceIdInfo id_info
  = catMaybes [arity_hsinfo, caf_hsinfo, strict_hsinfo, cpr_hsinfo,
               inline_hsinfo,  unfold_hsinfo, levity_hsinfo]
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
    sig_info = strictnessInfo id_info
    strict_hsinfo | not (isTopSig sig_info) = Just (HsStrictness sig_info)
                  | otherwise               = Nothing

    ------------  CPR --------------
    cpr_info = cprInfo id_info
    cpr_hsinfo | cpr_info /= topCprSig = Just (HsCpr cpr_info)
               | otherwise             = Nothing
    ------------  Unfolding  --------------
    unfold_hsinfo = toIfUnfolding loop_breaker (unfoldingInfo id_info)
    loop_breaker  = isStrongLoopBreaker (occInfo id_info)

    ------------  Inline prag  --------------
    inline_prag = inlinePragInfo id_info
    inline_hsinfo | isDefaultInlinePragma inline_prag = Nothing
                  | otherwise = Just (HsInline inline_prag)

    ------------  Levity polymorphism  ----------
    levity_hsinfo | isNeverLevPolyIdInfo id_info = Just HsLevity
                  | otherwise                    = Nothing

toIfaceJoinInfo :: Maybe JoinArity -> IfaceJoinInfo
toIfaceJoinInfo (Just ar) = IfaceJoinPoint ar
toIfaceJoinInfo Nothing   = IfaceNotJoinPoint

--------------------------
toIfUnfolding :: Bool -> Unfolding -> Maybe IfaceInfoItem
toIfUnfolding lb (CoreUnfolding { uf_tmpl = rhs
                                , uf_src = src
                                , uf_guidance = guidance })
  = Just $ HsUnfold lb $
    case src of
        InlineStable
          -> case guidance of
               UnfWhen {ug_arity = arity, ug_unsat_ok = unsat_ok, ug_boring_ok =  boring_ok }
                      -> IfInlineRule arity unsat_ok boring_ok if_rhs
               _other -> IfCoreUnfold True if_rhs
        InlineCompulsory -> IfCompulsory if_rhs
        InlineRhs        -> IfCoreUnfold False if_rhs
        -- Yes, even if guidance is UnfNever, expose the unfolding
        -- If we didn't want to expose the unfolding, GHC.Iface.Tidy would
        -- have stuck in NoUnfolding.  For supercompilation we want
        -- to see that unfolding!
  where
    if_rhs = toIfaceExpr rhs

toIfUnfolding lb (DFunUnfolding { df_bndrs = bndrs, df_args = args })
  = Just (HsUnfold lb (IfDFunUnfold (map toIfaceBndr bndrs) (map toIfaceExpr args)))
      -- No need to serialise the data constructor;
      -- we can recover it from the type of the dfun

toIfUnfolding _ (OtherCon {}) = Nothing
  -- The binding site of an Id doesn't have OtherCon, except perhaps
  -- where we have called zapUnfolding; and that evald'ness info is
  -- not needed by importing modules

toIfUnfolding _ BootUnfolding = Nothing
  -- Can't happen; we only have BootUnfolding for imported binders

toIfUnfolding _ NoUnfolding = Nothing

{-
************************************************************************
*                                                                      *
        Conversion of expressions
*                                                                      *
************************************************************************
-}

toIfaceExpr :: CoreExpr -> IfaceExpr
toIfaceExpr (Var v)         = toIfaceVar v
toIfaceExpr (Lit l)         = IfaceLit l
toIfaceExpr (Type ty)       = IfaceType (toIfaceType ty)
toIfaceExpr (Coercion co)   = IfaceCo   (toIfaceCoercion co)
toIfaceExpr (Lam x b)       = IfaceLam (toIfaceBndr x, toIfaceOneShot x) (toIfaceExpr b)
toIfaceExpr (App f a)       = toIfaceApp f [a]
toIfaceExpr (Case s x ty as)
  | null as                 = IfaceECase (toIfaceExpr s) (toIfaceType ty)
  | otherwise               = IfaceCase (toIfaceExpr s) (getOccFS x) (map toIfaceAlt as)
toIfaceExpr (Let b e)       = IfaceLet (toIfaceBind b) (toIfaceExpr e)
toIfaceExpr (Cast e co)     = IfaceCast (toIfaceExpr e) (toIfaceCoercion co)
toIfaceExpr (Tick t e)
  | Just t' <- toIfaceTickish t = IfaceTick t' (toIfaceExpr e)
  | otherwise                   = toIfaceExpr e

toIfaceOneShot :: Id -> IfaceOneShot
toIfaceOneShot id | isId id
                  , OneShotLam <- oneShotInfo (idInfo id)
                  = IfaceOneShot
                  | otherwise
                  = IfaceNoOneShot

---------------------
toIfaceTickish :: Tickish Id -> Maybe IfaceTickish
toIfaceTickish (ProfNote cc tick push) = Just (IfaceSCC cc tick push)
toIfaceTickish (HpcTick modl ix)       = Just (IfaceHpcTick modl ix)
toIfaceTickish (SourceNote src names)  = Just (IfaceSource src names)
toIfaceTickish (Breakpoint {})         = Nothing
   -- Ignore breakpoints, since they are relevant only to GHCi, and
   -- should not be serialised (#8333)

---------------------
toIfaceBind :: Bind Id -> IfaceBinding
toIfaceBind (NonRec b r) = IfaceNonRec (toIfaceLetBndr b) (toIfaceExpr r)
toIfaceBind (Rec prs)    = IfaceRec [(toIfaceLetBndr b, toIfaceExpr r) | (b,r) <- prs]

---------------------
toIfaceAlt :: (AltCon, [Var], CoreExpr)
           -> (IfaceConAlt, [FastString], IfaceExpr)
toIfaceAlt (c,bs,r) = (toIfaceCon c, map getOccFS bs, toIfaceExpr r)

---------------------
toIfaceCon :: AltCon -> IfaceConAlt
toIfaceCon (DataAlt dc) = IfaceDataAlt (getName dc)
toIfaceCon (LitAlt l)   = IfaceLitAlt l
toIfaceCon DEFAULT      = IfaceDefault

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
      IfaceApp (IfaceApp (IfaceExt noinlineIdName)
                         (IfaceType (toIfaceType (idType v))))
               (IfaceExt name) -- don't use mkIfaceApps, or infinite loop

    | Just fcall <- isFCallId_maybe v = IfaceFCall fcall (toIfaceType (idType v))
                                      -- Foreign calls have special syntax

    | isExternalName name             = IfaceExt name
    | otherwise                       = IfaceLcl (getOccFS name)
  where name = idName v


---------------------
toIfaceLFInfo :: LambdaFormInfo -> IfaceLFInfo
toIfaceLFInfo lfi = case lfi of
    LFReEntrant _ _ arity _ _ ->
      IfLFReEntrant arity
    LFThunk _ _ updatable sfi mb_fun ->
      IfLFThunk updatable (toIfaceStandardFormInfo sfi) mb_fun
    LFCon dc ->
      IfLFCon (dataConName dc)
    LFUnknown mb_fun ->
      IfLFUnknown mb_fun
    LFUnlifted ->
      IfLFUnlifted
    LFLetNoEscape ->
      panic "toIfaceLFInfo: LFLetNoEscape"

toIfaceStandardFormInfo :: StandardFormInfo -> IfaceStandardFormInfo
toIfaceStandardFormInfo NonStandardThunk = IfStandardFormInfo 1
toIfaceStandardFormInfo sf =
    IfStandardFormInfo $!
      tag sf .|. encodeField (field sf)
  where
    tag SelectorThunk{} = 0
    tag ApThunk{} = setBit 0 1
    tag NonStandardThunk = panic "toIfaceStandardFormInfo: NonStandardThunk"

    field (SelectorThunk n) = n
    field (ApThunk n) = n
    field NonStandardThunk = panic "toIfaceStandardFormInfo: NonStandardThunk"

    encodeField n =
      let wn = fromIntegral n :: Word
          shifted = wn `unsafeShiftL` 2
      in ASSERT(shifted > 0 && shifted < fromIntegral (maxBound :: Word16))
         (fromIntegral shifted :: Word16)


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
    to the reference.

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

-}
