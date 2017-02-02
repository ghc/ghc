{-# LANGUAGE CPP #-}

-- | Functions for converting Core things to interface file things.
module ToIface
    ( -- * Binders
      toIfaceTvBndr
    , toIfaceTvBndrs
    , toIfaceIdBndr
    , toIfaceBndr
    , toIfaceForAllBndr
    , toIfaceTyVarBinders
    , toIfaceTyVar
      -- * Types
    , toIfaceType
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
    , toIfaceCoercion
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
    , toIfaceOneShot
    , toIfaceTickish
    , toIfaceBind
    , toIfaceAlt
    , toIfaceCon
    , toIfaceApp
    , toIfaceVar
    ) where

#include "HsVersions.h"

import IfaceSyn
import DataCon
import Id
import IdInfo
import CoreSyn
import TyCon hiding ( pprPromotionQuote )
import CoAxiom
import TysPrim ( eqPrimTyCon, eqReprPrimTyCon )
import TysWiredIn ( heqTyCon )
import MkId ( noinlineIdName )
import PrelNames
import Name
import BasicTypes
import Type
import PatSyn
import Outputable
import FastString
import Util
import Var
import VarEnv
import TyCoRep
import Demand ( isTopSig )

import Data.Maybe ( catMaybes )

----------------
toIfaceTvBndr :: TyVar -> IfaceTvBndr
toIfaceTvBndr tyvar   = ( occNameFS (getOccName tyvar)
                        , toIfaceKind (tyVarKind tyvar)
                        )

toIfaceIdBndr :: Id -> (IfLclName, IfaceType)
toIfaceIdBndr id      = (occNameFS (getOccName id),    toIfaceType (idType id))

toIfaceTvBndrs :: [TyVar] -> [IfaceTvBndr]
toIfaceTvBndrs = map toIfaceTvBndr

toIfaceBndr :: Var -> IfaceBndr
toIfaceBndr var
  | isId var  = IfaceIdBndr (toIfaceIdBndr var)
  | otherwise = IfaceTvBndr (toIfaceTvBndr var)

toIfaceTyVarBinder :: TyVarBndr TyVar vis -> TyVarBndr IfaceTvBndr vis
toIfaceTyVarBinder (TvBndr tv vis) = TvBndr (toIfaceTvBndr tv) vis

toIfaceTyVarBinders :: [TyVarBndr TyVar vis] -> [TyVarBndr IfaceTvBndr vis]
toIfaceTyVarBinders = map toIfaceTyVarBinder

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
-- Synonyms are retained in the interface type
toIfaceType (TyVarTy tv)   -- See Note [TcTyVars in IfaceType] in IfaceType
  | isTcTyVar tv            = IfaceTcTyVar tv
  | otherwise               = IfaceTyVar (toIfaceTyVar tv)
toIfaceType (AppTy t1 t2)   = IfaceAppTy (toIfaceType t1) (toIfaceType t2)
toIfaceType (LitTy n)       = IfaceLitTy (toIfaceTyLit n)
toIfaceType (ForAllTy b t)  = IfaceForAllTy (toIfaceForAllBndr b) (toIfaceType t)
toIfaceType (FunTy t1 t2)
  | isPredTy t1             = IfaceDFunTy (toIfaceType t1) (toIfaceType t2)
  | otherwise               = IfaceFunTy  (toIfaceType t1) (toIfaceType t2)
toIfaceType (CastTy ty co)  = IfaceCastTy (toIfaceType ty) (toIfaceCoercion co)
toIfaceType (CoercionTy co) = IfaceCoercionTy (toIfaceCoercion co)

toIfaceType (TyConApp tc tys)
    -- tuples
  | Just sort <- tyConTuple_maybe tc
  , n_tys == arity
  = IfaceTupleTy sort IsNotPromoted (toIfaceTcArgs tc tys)

  | Just dc <- isPromotedDataCon_maybe tc
  , isTupleDataCon dc
  , n_tys == 2*arity
  = IfaceTupleTy BoxedTuple IsPromoted (toIfaceTcArgs tc (drop arity tys))

    -- type equalities: see Note [Equality predicates in IfaceType]
  | tyConName tc == eqTyConName
  = let info = IfaceTyConInfo IsNotPromoted (IfaceEqualityTyCon True)
    in IfaceTyConApp (IfaceTyCon (tyConName tc) info) (toIfaceTcArgs tc tys)

  | tc `elem` [ eqPrimTyCon, eqReprPrimTyCon, heqTyCon ]
  , [k1, k2, _t1, _t2] <- tys
  = let homogeneous = k1 `eqType` k2
        info = IfaceTyConInfo IsNotPromoted (IfaceEqualityTyCon homogeneous)
    in IfaceTyConApp (IfaceTyCon (tyConName tc) info) (toIfaceTcArgs tc tys)

    -- other applications
  | otherwise
  = IfaceTyConApp (toIfaceTyCon tc) (toIfaceTcArgs tc tys)
  where
    arity = tyConArity tc
    n_tys = length tys

toIfaceTyVar :: TyVar -> FastString
toIfaceTyVar = occNameFS . getOccName

toIfaceCoVar :: CoVar -> FastString
toIfaceCoVar = occNameFS . getOccName

toIfaceForAllBndr :: TyVarBinder -> IfaceForAllBndr
toIfaceForAllBndr (TvBndr v vis) = TvBndr (toIfaceTvBndr v) vis

----------------
toIfaceTyCon :: TyCon -> IfaceTyCon
toIfaceTyCon tc
  = IfaceTyCon tc_name info
  where
    tc_name = tyConName tc
    info    = IfaceTyConInfo promoted sort
    promoted | isPromotedDataCon tc = IsPromoted
             | otherwise            = IsNotPromoted

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
  where info = IfaceTyConInfo IsNotPromoted IfaceNormalTyCon
  -- Used for the "rough-match" tycon stuff,
  -- where pretty-printing is not an issue

toIfaceTyLit :: TyLit -> IfaceTyLit
toIfaceTyLit (NumTyLit x) = IfaceNumTyLit x
toIfaceTyLit (StrTyLit x) = IfaceStrTyLit x

----------------
toIfaceCoercion :: Coercion -> IfaceCoercion
toIfaceCoercion (Refl r ty)         = IfaceReflCo r (toIfaceType ty)
toIfaceCoercion co@(TyConAppCo r tc cos)
  | tc `hasKey` funTyConKey
  , [_,_,_,_] <- cos                = pprPanic "toIfaceCoercion" (ppr co)
  | otherwise                       = IfaceTyConAppCo r (toIfaceTyCon tc)
                                                        (map toIfaceCoercion cos)
toIfaceCoercion (AppCo co1 co2)     = IfaceAppCo  (toIfaceCoercion co1)
                                                  (toIfaceCoercion co2)
toIfaceCoercion (ForAllCo tv k co)  = IfaceForAllCo (toIfaceTvBndr tv)
                                                    (toIfaceCoercion k)
                                                    (toIfaceCoercion co)
toIfaceCoercion (FunCo r co1 co2)   = IfaceFunCo r (toIfaceCoercion co1)
                                                   (toIfaceCoercion co2)
toIfaceCoercion (CoVarCo cv)        = IfaceCoVarCo  (toIfaceCoVar cv)
toIfaceCoercion (AxiomInstCo con ind cos)
                                    = IfaceAxiomInstCo (coAxiomName con) ind
                                                       (map toIfaceCoercion cos)
toIfaceCoercion (UnivCo p r t1 t2)  = IfaceUnivCo (toIfaceUnivCoProv p) r
                                                  (toIfaceType t1)
                                                  (toIfaceType t2)
toIfaceCoercion (SymCo co)          = IfaceSymCo (toIfaceCoercion co)
toIfaceCoercion (TransCo co1 co2)   = IfaceTransCo (toIfaceCoercion co1)
                                                   (toIfaceCoercion co2)
toIfaceCoercion (NthCo d co)        = IfaceNthCo d (toIfaceCoercion co)
toIfaceCoercion (LRCo lr co)        = IfaceLRCo lr (toIfaceCoercion co)
toIfaceCoercion (InstCo co arg)     = IfaceInstCo (toIfaceCoercion co)
                                                  (toIfaceCoercion arg)
toIfaceCoercion (CoherenceCo c1 c2) = IfaceCoherenceCo (toIfaceCoercion c1)
                                                       (toIfaceCoercion c2)
toIfaceCoercion (KindCo c)          = IfaceKindCo (toIfaceCoercion c)
toIfaceCoercion (SubCo co)          = IfaceSubCo (toIfaceCoercion co)
toIfaceCoercion (AxiomRuleCo co cs) = IfaceAxiomRuleCo (coaxrName co)
                                          (map toIfaceCoercion cs)

toIfaceUnivCoProv :: UnivCoProvenance -> IfaceUnivCoProv
toIfaceUnivCoProv UnsafeCoerceProv    = IfaceUnsafeCoerceProv
toIfaceUnivCoProv (PhantomProv co)    = IfacePhantomProv (toIfaceCoercion co)
toIfaceUnivCoProv (ProofIrrelProv co) = IfaceProofIrrelProv (toIfaceCoercion co)
toIfaceUnivCoProv (PluginProv str)    = IfacePluginProv str
toIfaceUnivCoProv (HoleProv h)        = IfaceHoleProv (chUnique h)

toIfaceTcArgs :: TyCon -> [Type] -> IfaceTcArgs
-- See Note [Suppressing invisible arguments]
toIfaceTcArgs tc ty_args
  = go (mkEmptyTCvSubst in_scope) (tyConKind tc) ty_args
  where
    in_scope = mkInScopeSet (tyCoVarsOfTypes ty_args)

    go _   _                   []     = ITC_Nil
    go env ty                  ts
      | Just ty' <- coreView ty
      = go env ty' ts
    go env (ForAllTy (TvBndr tv vis) res) (t:ts)
      | isVisibleArgFlag vis = ITC_Vis   t' ts'
      | otherwise            = ITC_Invis t' ts'
      where
        t'  = toIfaceType t
        ts' = go (extendTvSubst env tv t) res ts

    go env (FunTy _ res) (t:ts) -- No type-class args in tycon apps
      = ITC_Vis (toIfaceType t) (go env res ts)

    go env (TyVarTy tv) ts
      | Just ki <- lookupTyVar env tv = go env ki ts
    go env kind (t:ts) = WARN( True, ppr tc $$ ppr (tyConKind tc) $$ ppr ty_args )
                         ITC_Vis (toIfaceType t) (go env kind ts) -- Ill-kinded

tidyToIfaceType :: TidyEnv -> Type -> IfaceType
tidyToIfaceType env ty = toIfaceType (tidyType env ty)

tidyToIfaceTcArgs :: TidyEnv -> TyCon -> [Type] -> IfaceTcArgs
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
    (env1, univ_bndrs') = tidyTyVarBinders emptyTidyEnv univ_bndrs
    (env2, ex_bndrs')   = tidyTyVarBinders env1 ex_bndrs
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
  -- Put into the interface file any IdInfo that CoreTidy.tidyLetBndr
  -- has left on the Id.  See Note [IdInfo on nested let-bindings] in IfaceSyn

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
  = case catMaybes [arity_hsinfo, caf_hsinfo, strict_hsinfo,
                    inline_hsinfo,  unfold_hsinfo, levity_hsinfo] of
       []    -> NoInfo
       infos -> HasInfo infos
               -- NB: strictness and arity must appear in the list before unfolding
               -- See TcIface.tcUnfolding
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
        -- If we didn't want to expose the unfolding, TidyPgm would
        -- have stuck in NoUnfolding.  For supercompilation we want
        -- to see that unfolding!
  where
    if_rhs = toIfaceExpr rhs

toIfUnfolding lb (DFunUnfolding { df_bndrs = bndrs, df_args = args })
  = Just (HsUnfold lb (IfDFunUnfold (map toIfaceBndr bndrs) (map toIfaceExpr args)))
      -- No need to serialise the data constructor;
      -- we can recover it from the type of the dfun

toIfUnfolding _ _
  = Nothing

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
   -- should not be serialised (Trac #8333)

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
mkIfaceApps f as = foldl (\f a -> IfaceApp f (toIfaceExpr a)) f as

---------------------
toIfaceVar :: Id -> IfaceExpr
toIfaceVar v
    | Just fcall <- isFCallId_maybe v            = IfaceFCall fcall (toIfaceType (idType v))
       -- Foreign calls have special syntax
    | isBootUnfolding (idUnfolding v)
    = IfaceApp (IfaceApp (IfaceExt noinlineIdName) (IfaceType (toIfaceType (idType v))))
               (IfaceExt name) -- don't use mkIfaceApps, or infinite loop
       -- See Note [Inlining and hs-boot files]
    | isExternalName name                        = IfaceExt name
    | otherwise                                  = IfaceLcl (getOccFS name)
  where name = idName v


{- Note [Inlining and hs-boot files]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this example (Trac #10083, #12789):

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
slower by 8% overall (on Trac #9872a-d, and T1969: the reason
is that these NOINLINE'd functions now can't be profitably inlined
outside of the hs-boot loop.

-}
