{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1996-1998


TcHsSyn: Specialisations of the @HsSyn@ syntax for the typechecker

This module is an extension of @HsSyn@ syntax, for use in the type
checker.
-}

{-# LANGUAGE CPP #-}

module TcHsSyn (
        mkHsConApp, mkHsDictLet, mkHsApp,
        hsLitType, hsLPatType, hsPatType,
        mkHsAppTy, mkSimpleHsAlt,
        nlHsIntLit,
        shortCutLit, hsOverLitName,
        conLikeResTy,

        -- re-exported from TcMonad
        TcId, TcIdSet,

        zonkTopDecls, zonkTopExpr, zonkTopLExpr,
        zonkTopBndrs, zonkTyBndrsX,
        emptyZonkEnv, mkEmptyZonkEnv,
        zonkTcTypeToType, zonkTcTypeToTypes, zonkTyVarOcc,
        zonkCoToCo
  ) where

#include "HsVersions.h"

import HsSyn
import Id
import TcRnMonad
import PrelNames
import TcType
import TcMType
import TcEvidence
import TysPrim
import TysWiredIn
import Type
import Coercion
import ConLike
import DataCon
import Name
import Var
import VarSet
import VarEnv
import DynFlags
import Literal
import BasicTypes
import Maybes
import SrcLoc
import Bag
import Outputable
import Util
import qualified GHC.LanguageExtensions as LangExt

#if __GLASGOW_HASKELL__ < 709
import Data.Traversable ( traverse )
#endif
import Control.Monad
import Data.List  ( partition )

{-
************************************************************************
*                                                                      *
\subsection[mkFailurePair]{Code for pattern-matching and other failures}
*                                                                      *
************************************************************************

Note: If @hsLPatType@ doesn't bear a strong resemblance to @exprType@,
then something is wrong.
-}

hsLPatType :: OutPat Id -> Type
hsLPatType (L _ pat) = hsPatType pat

hsPatType :: Pat Id -> Type
hsPatType (ParPat pat)                = hsLPatType pat
hsPatType (WildPat ty)                = ty
hsPatType (VarPat (L _ var))          = idType var
hsPatType (BangPat pat)               = hsLPatType pat
hsPatType (LazyPat pat)               = hsLPatType pat
hsPatType (LitPat lit)                = hsLitType lit
hsPatType (AsPat var _)               = idType (unLoc var)
hsPatType (ViewPat _ _ ty)            = ty
hsPatType (ListPat _ ty Nothing)      = mkListTy ty
hsPatType (ListPat _ _ (Just (ty,_))) = ty
hsPatType (PArrPat _ ty)              = mkPArrTy ty
hsPatType (TuplePat _ bx tys)         = mkTupleTy bx tys
hsPatType (ConPatOut { pat_con = L _ con, pat_arg_tys = tys })
                                      = conLikeResTy con tys
hsPatType (SigPatOut _ ty)            = ty
hsPatType (NPat (L _ lit) _ _)        = overLitType lit
hsPatType (NPlusKPat id _ _ _)        = idType (unLoc id)
hsPatType (CoPat _ _ ty)              = ty
hsPatType p                           = pprPanic "hsPatType" (ppr p)


hsLitType :: HsLit -> TcType
hsLitType (HsChar _ _)       = charTy
hsLitType (HsCharPrim _ _)   = charPrimTy
hsLitType (HsString _ _)     = stringTy
hsLitType (HsStringPrim _ _) = addrPrimTy
hsLitType (HsInt _ _)        = intTy
hsLitType (HsIntPrim _ _)    = intPrimTy
hsLitType (HsWordPrim _ _)   = wordPrimTy
hsLitType (HsInt64Prim _ _)  = int64PrimTy
hsLitType (HsWord64Prim _ _) = word64PrimTy
hsLitType (HsInteger _ _ ty) = ty
hsLitType (HsRat _ ty)       = ty
hsLitType (HsFloatPrim _)    = floatPrimTy
hsLitType (HsDoublePrim _)   = doublePrimTy

-- Overloaded literals. Here mainly because it uses isIntTy etc

shortCutLit :: DynFlags -> OverLitVal -> TcType -> Maybe (HsExpr TcId)
shortCutLit dflags (HsIntegral src i) ty
  | isIntTy ty  && inIntRange  dflags i = Just (HsLit (HsInt src i))
  | isWordTy ty && inWordRange dflags i
                                   = Just (mkLit wordDataCon (HsWordPrim src i))
  | isIntegerTy ty = Just (HsLit (HsInteger src i ty))
  | otherwise = shortCutLit dflags (HsFractional (integralFractionalLit i)) ty
        -- The 'otherwise' case is important
        -- Consider (3 :: Float).  Syntactically it looks like an IntLit,
        -- so we'll call shortCutIntLit, but of course it's a float
        -- This can make a big difference for programs with a lot of
        -- literals, compiled without -O

shortCutLit _ (HsFractional f) ty
  | isFloatTy ty  = Just (mkLit floatDataCon  (HsFloatPrim f))
  | isDoubleTy ty = Just (mkLit doubleDataCon (HsDoublePrim f))
  | otherwise     = Nothing

shortCutLit _ (HsIsString src s) ty
  | isStringTy ty = Just (HsLit (HsString src s))
  | otherwise     = Nothing

mkLit :: DataCon -> HsLit -> HsExpr Id
mkLit con lit = HsApp (nlHsVar (dataConWrapId con)) (nlHsLit lit)

------------------------------
hsOverLitName :: OverLitVal -> Name
-- Get the canonical 'fromX' name for a particular OverLitVal
hsOverLitName (HsIntegral {})   = fromIntegerName
hsOverLitName (HsFractional {}) = fromRationalName
hsOverLitName (HsIsString {})   = fromStringName

{-
************************************************************************
*                                                                      *
\subsection[BackSubst-HsBinds]{Running a substitution over @HsBinds@}
*                                                                      *
************************************************************************

The rest of the zonking is done *after* typechecking.
The main zonking pass runs over the bindings

 a) to convert TcTyVars to TyVars etc, dereferencing any bindings etc
 b) convert unbound TcTyVar to Void
 c) convert each TcId to an Id by zonking its type

The type variables are converted by binding mutable tyvars to immutable ones
and then zonking as normal.

The Ids are converted by binding them in the normal Tc envt; that
way we maintain sharing; eg an Id is zonked at its binding site and they
all occurrences of that Id point to the common zonked copy

It's all pretty boring stuff, because HsSyn is such a large type, and
the environment manipulation is tiresome.
-}

type UnboundTyVarZonker = TcTyVar -> TcM Type
        -- How to zonk an unbound type variable
        -- Note [Zonking the LHS of a RULE]

-- | A ZonkEnv carries around several bits.
-- The UnboundTyVarZonker just zaps unbouned meta-tyvars to Any (as
-- defined in zonkTypeZapping), except on the LHS of rules. See
-- Note [Zonking the LHS of a RULE]. The (TyCoVarEnv TyVar) and is just
-- an optimisation: when binding a tyvar or covar, we zonk the kind right away
-- and add a mapping to the env. This prevents re-zonking the kind at
-- every occurrence. But this is *just* an optimisation.
-- The final (IdEnv Var) optimises zonking for
-- Ids. It is knot-tied. We must be careful never to put coercion variables
-- (which are Ids, after all) in the knot-tied env, because coercions can
-- appear in types, and we sometimes inspect a zonked type in this module.
data ZonkEnv
  = ZonkEnv
      UnboundTyVarZonker
      (TyCoVarEnv TyVar)
      (IdEnv      Var)         -- What variables are in scope
        -- Maps an Id or EvVar to its zonked version; both have the same Name
        -- Note that all evidence (coercion variables as well as dictionaries)
        --      are kept in the ZonkEnv
        -- Only *type* abstraction is done by side effect
        -- Is only consulted lazily; hence knot-tying

instance Outputable ZonkEnv where
  ppr (ZonkEnv _ _ty_env var_env) = vcat (map ppr (varEnvElts var_env))


-- The EvBinds have to already be zonked, but that's usually the case.
emptyZonkEnv :: ZonkEnv
emptyZonkEnv = mkEmptyZonkEnv zonkTypeZapping

mkEmptyZonkEnv :: UnboundTyVarZonker -> ZonkEnv
mkEmptyZonkEnv zonker = ZonkEnv zonker emptyVarEnv emptyVarEnv

-- | Extend the knot-tied environment.
extendIdZonkEnvRec :: ZonkEnv -> [Var] -> ZonkEnv
extendIdZonkEnvRec (ZonkEnv zonk_ty ty_env id_env) ids
    -- NB: Don't look at the var to decide which env't to put it in. That
    -- would end up knot-tying all the env'ts.
  = ZonkEnv zonk_ty ty_env (extendVarEnvList id_env [(id,id) | id <- ids])
  -- Given coercion variables will actually end up here. That's OK though:
  -- coercion variables are never looked up in the knot-tied env't, so zonking
  -- them simply doesn't get optimised. No one gets hurt. An improvement (?)
  -- would be to do SCC analysis in zonkEvBinds and then only knot-tie the
  -- recursive groups. But perhaps the time it takes to do the analysis is
  -- more than the savings.

extendZonkEnv :: ZonkEnv -> [Var] -> ZonkEnv
extendZonkEnv (ZonkEnv zonk_ty tyco_env id_env) vars
  = ZonkEnv zonk_ty (extendVarEnvList tyco_env [(tv,tv) | tv <- tycovars])
                    (extendVarEnvList id_env   [(id,id) | id <- ids])
  where (tycovars, ids) = partition isTyCoVar vars

extendIdZonkEnv1 :: ZonkEnv -> Var -> ZonkEnv
extendIdZonkEnv1 (ZonkEnv zonk_ty ty_env id_env) id
  = ZonkEnv zonk_ty ty_env (extendVarEnv id_env id id)

extendTyZonkEnv1 :: ZonkEnv -> TyVar -> ZonkEnv
extendTyZonkEnv1 (ZonkEnv zonk_ty ty_env id_env) ty
  = ZonkEnv zonk_ty (extendVarEnv ty_env ty ty) id_env

setZonkType :: ZonkEnv -> UnboundTyVarZonker -> ZonkEnv
setZonkType (ZonkEnv _ ty_env id_env) zonk_ty
  = ZonkEnv zonk_ty ty_env id_env

zonkEnvIds :: ZonkEnv -> [Id]
zonkEnvIds (ZonkEnv _ _ id_env) = varEnvElts id_env

zonkIdOcc :: ZonkEnv -> TcId -> Id
-- Ids defined in this module should be in the envt;
-- ignore others.  (Actually, data constructors are also
-- not LocalVars, even when locally defined, but that is fine.)
-- (Also foreign-imported things aren't currently in the ZonkEnv;
--  that's ok because they don't need zonking.)
--
-- Actually, Template Haskell works in 'chunks' of declarations, and
-- an earlier chunk won't be in the 'env' that the zonking phase
-- carries around.  Instead it'll be in the tcg_gbl_env, already fully
-- zonked.  There's no point in looking it up there (except for error
-- checking), and it's not conveniently to hand; hence the simple
-- 'orElse' case in the LocalVar branch.
--
-- Even without template splices, in module Main, the checking of
-- 'main' is done as a separate chunk.
zonkIdOcc (ZonkEnv _zonk_ty _ty_env id_env) id
  | isLocalVar id = lookupVarEnv id_env id `orElse`
                    id
  | otherwise     = id

zonkIdOccs :: ZonkEnv -> [TcId] -> [Id]
zonkIdOccs env ids = map (zonkIdOcc env) ids

-- zonkIdBndr is used *after* typechecking to get the Id's type
-- to its final form.  The TyVarEnv give
zonkIdBndr :: ZonkEnv -> TcId -> TcM Id
zonkIdBndr env id
  = do ty' <- zonkTcTypeToType env (idType id)
       return (setIdType id ty')

zonkIdBndrs :: ZonkEnv -> [TcId] -> TcM [Id]
zonkIdBndrs env ids = mapM (zonkIdBndr env) ids

zonkTopBndrs :: [TcId] -> TcM [Id]
zonkTopBndrs ids = zonkIdBndrs emptyZonkEnv ids

zonkFieldOcc :: ZonkEnv -> FieldOcc TcId -> TcM (FieldOcc Id)
zonkFieldOcc env (FieldOcc lbl sel) = fmap (FieldOcc lbl) $ zonkIdBndr env sel

zonkEvBndrsX :: ZonkEnv -> [EvVar] -> TcM (ZonkEnv, [Var])
zonkEvBndrsX = mapAccumLM zonkEvBndrX

zonkEvBndrX :: ZonkEnv -> EvVar -> TcM (ZonkEnv, EvVar)
-- Works for dictionaries and coercions
zonkEvBndrX env var
  = do { var' <- zonkEvBndr env var
       ; return (extendZonkEnv env [var'], var') }

zonkEvBndr :: ZonkEnv -> EvVar -> TcM EvVar
-- Works for dictionaries and coercions
-- Does not extend the ZonkEnv
zonkEvBndr env var
  = do { let var_ty = varType var
       ; ty <-
           {-# SCC "zonkEvBndr_zonkTcTypeToType" #-}
           zonkTcTypeToType env var_ty
       ; return (setVarType var ty) }

zonkEvVarOcc :: ZonkEnv -> EvVar -> TcM EvTerm
zonkEvVarOcc env v
  | isCoVar v
  = EvCoercion <$> zonkCoVarOcc env v
  | otherwise
  = return (EvId $ zonkIdOcc env v)

zonkTyBndrsX :: ZonkEnv -> [TyVar] -> TcM (ZonkEnv, [TyVar])
zonkTyBndrsX = mapAccumLM zonkTyBndrX

zonkTyBndrX :: ZonkEnv -> TyVar -> TcM (ZonkEnv, TyVar)
-- This guarantees to return a TyVar (not a TcTyVar)
-- then we add it to the envt, so all occurrences are replaced
zonkTyBndrX env tv
  = ASSERT( isImmutableTyVar tv )
    do { ki <- zonkTcTypeToType env (tyVarKind tv)
               -- Internal names tidy up better, for iface files.
       ; let tv' = mkTyVar (tyVarName tv) ki
       ; return (extendTyZonkEnv1 env tv', tv') }

zonkTopExpr :: HsExpr TcId -> TcM (HsExpr Id)
zonkTopExpr e = zonkExpr emptyZonkEnv e

zonkTopLExpr :: LHsExpr TcId -> TcM (LHsExpr Id)
zonkTopLExpr e = zonkLExpr emptyZonkEnv e

zonkTopDecls :: Bag EvBind
             -> LHsBinds TcId
             -> [LRuleDecl TcId] -> [LVectDecl TcId] -> [LTcSpecPrag] -> [LForeignDecl TcId]
             -> TcM ([Id],
                     Bag EvBind,
                     LHsBinds Id,
                     [LForeignDecl Id],
                     [LTcSpecPrag],
                     [LRuleDecl    Id],
                     [LVectDecl    Id])
zonkTopDecls ev_binds binds rules vects imp_specs fords
  = do  { (env1, ev_binds') <- zonkEvBinds emptyZonkEnv ev_binds
        ; (env2, binds') <- zonkRecMonoBinds env1 binds
                        -- Top level is implicitly recursive
        ; rules' <- zonkRules env2 rules
        ; vects' <- zonkVects env2 vects
        ; specs' <- zonkLTcSpecPrags env2 imp_specs
        ; fords' <- zonkForeignExports env2 fords
        ; return (zonkEnvIds env2, ev_binds', binds', fords', specs', rules', vects') }

---------------------------------------------
zonkLocalBinds :: ZonkEnv -> HsLocalBinds TcId -> TcM (ZonkEnv, HsLocalBinds Id)
zonkLocalBinds env EmptyLocalBinds
  = return (env, EmptyLocalBinds)

zonkLocalBinds _ (HsValBinds (ValBindsIn {}))
  = panic "zonkLocalBinds" -- Not in typechecker output

zonkLocalBinds env (HsValBinds (ValBindsOut binds sigs))
  = do  { (env1, new_binds) <- go env binds
        ; return (env1, HsValBinds (ValBindsOut new_binds sigs)) }
  where
    go env []
      = return (env, [])
    go env ((r,b):bs)
      = do { (env1, b')  <- zonkRecMonoBinds env b
           ; (env2, bs') <- go env1 bs
           ; return (env2, (r,b'):bs') }

zonkLocalBinds env (HsIPBinds (IPBinds binds dict_binds)) = do
    new_binds <- mapM (wrapLocM zonk_ip_bind) binds
    let
        env1 = extendIdZonkEnvRec env [ n | L _ (IPBind (Right n) _) <- new_binds]
    (env2, new_dict_binds) <- zonkTcEvBinds env1 dict_binds
    return (env2, HsIPBinds (IPBinds new_binds new_dict_binds))
  where
    zonk_ip_bind (IPBind n e)
        = do n' <- mapIPNameTc (zonkIdBndr env) n
             e' <- zonkLExpr env e
             return (IPBind n' e')

---------------------------------------------
zonkRecMonoBinds :: ZonkEnv -> LHsBinds TcId -> TcM (ZonkEnv, LHsBinds Id)
zonkRecMonoBinds env binds
 = fixM (\ ~(_, new_binds) -> do
        { let env1 = extendIdZonkEnvRec env (collectHsBindsBinders new_binds)
        ; binds' <- zonkMonoBinds env1 binds
        ; return (env1, binds') })

---------------------------------------------
zonkMonoBinds :: ZonkEnv -> LHsBinds TcId -> TcM (LHsBinds Id)
zonkMonoBinds env binds = mapBagM (zonk_lbind env) binds

zonk_lbind :: ZonkEnv -> LHsBind TcId -> TcM (LHsBind Id)
zonk_lbind env = wrapLocM (zonk_bind env)

zonk_bind :: ZonkEnv -> HsBind TcId -> TcM (HsBind Id)
zonk_bind env bind@(PatBind { pat_lhs = pat, pat_rhs = grhss, pat_rhs_ty = ty})
  = do  { (_env, new_pat) <- zonkPat env pat            -- Env already extended
        ; new_grhss <- zonkGRHSs env zonkLExpr grhss
        ; new_ty    <- zonkTcTypeToType env ty
        ; return (bind { pat_lhs = new_pat, pat_rhs = new_grhss, pat_rhs_ty = new_ty }) }

zonk_bind env (VarBind { var_id = var, var_rhs = expr, var_inline = inl })
  = do { new_var  <- zonkIdBndr env var
       ; new_expr <- zonkLExpr env expr
       ; return (VarBind { var_id = new_var, var_rhs = new_expr, var_inline = inl }) }

zonk_bind env bind@(FunBind { fun_id = L loc var, fun_matches = ms
                            , fun_co_fn = co_fn })
  = do { new_var <- zonkIdBndr env var
       ; (env1, new_co_fn) <- zonkCoFn env co_fn
       ; new_ms <- zonkMatchGroup env1 zonkLExpr ms
       ; return (bind { fun_id = L loc new_var, fun_matches = new_ms
                      , fun_co_fn = new_co_fn }) }

zonk_bind env (AbsBinds { abs_tvs = tyvars, abs_ev_vars = evs
                        , abs_ev_binds = ev_binds
                        , abs_exports = exports
                        , abs_binds = val_binds })
  = ASSERT( all isImmutableTyVar tyvars )
    do { (env0, new_tyvars) <- zonkTyBndrsX env tyvars
       ; (env1, new_evs) <- zonkEvBndrsX env0 evs
       ; (env2, new_ev_binds) <- zonkTcEvBinds_s env1 ev_binds
       ; (new_val_bind, new_exports) <- fixM $ \ ~(new_val_binds, _) ->
         do { let env3 = extendIdZonkEnvRec env2
                           (collectHsBindsBinders new_val_binds)
            ; new_val_binds <- zonkMonoBinds env3 val_binds
            ; new_exports   <- mapM (zonkExport env3) exports
            ; return (new_val_binds, new_exports) }
       ; return (AbsBinds { abs_tvs = new_tyvars, abs_ev_vars = new_evs
                          , abs_ev_binds = new_ev_binds
                          , abs_exports = new_exports, abs_binds = new_val_bind }) }
  where
    zonkExport env (ABE{ abe_wrap = wrap, abe_inst_wrap = inst_wrap
                       , abe_poly = poly_id
                       , abe_mono = mono_id, abe_prags = prags })
        = do new_poly_id <- zonkIdBndr env poly_id
             (_, new_wrap) <- zonkCoFn env wrap
             (_, new_inst_wrap) <- zonkCoFn env inst_wrap
             new_prags <- zonkSpecPrags env prags
             return (ABE{ abe_wrap = new_wrap, abe_inst_wrap = new_inst_wrap
                        , abe_poly = new_poly_id
                        , abe_mono = zonkIdOcc env mono_id
                        , abe_prags = new_prags })

zonk_bind env (PatSynBind bind@(PSB { psb_id = L loc id
                                    , psb_args = details
                                    , psb_def = lpat
                                    , psb_dir = dir }))
  = do { id' <- zonkIdBndr env id
       ; details' <- zonkPatSynDetails env details
       ; (env1, lpat') <- zonkPat env lpat
       ; (_env2, dir') <- zonkPatSynDir env1 dir
       ; return $ PatSynBind $
                  bind { psb_id = L loc id'
                       , psb_args = details'
                       , psb_def = lpat'
                       , psb_dir = dir' } }

zonkPatSynDetails :: ZonkEnv
                  -> HsPatSynDetails (Located TcId)
                  -> TcM (HsPatSynDetails (Located Id))
zonkPatSynDetails env = traverse (wrapLocM $ zonkIdBndr env)

zonkPatSynDir :: ZonkEnv -> HsPatSynDir TcId -> TcM (ZonkEnv, HsPatSynDir Id)
zonkPatSynDir env Unidirectional = return (env, Unidirectional)
zonkPatSynDir env ImplicitBidirectional = return (env, ImplicitBidirectional)
zonkPatSynDir env (ExplicitBidirectional mg) = do
    mg' <- zonkMatchGroup env zonkLExpr mg
    return (env, ExplicitBidirectional mg')

zonkSpecPrags :: ZonkEnv -> TcSpecPrags -> TcM TcSpecPrags
zonkSpecPrags _   IsDefaultMethod = return IsDefaultMethod
zonkSpecPrags env (SpecPrags ps)  = do { ps' <- zonkLTcSpecPrags env ps
                                       ; return (SpecPrags ps') }

zonkLTcSpecPrags :: ZonkEnv -> [LTcSpecPrag] -> TcM [LTcSpecPrag]
zonkLTcSpecPrags env ps
  = mapM zonk_prag ps
  where
    zonk_prag (L loc (SpecPrag id co_fn inl))
        = do { (_, co_fn') <- zonkCoFn env co_fn
             ; return (L loc (SpecPrag (zonkIdOcc env id) co_fn' inl)) }

{-
************************************************************************
*                                                                      *
\subsection[BackSubst-Match-GRHSs]{Match and GRHSs}
*                                                                      *
************************************************************************
-}

zonkMatchGroup :: ZonkEnv
               -> (ZonkEnv -> Located (body TcId) -> TcM (Located (body Id)))
               -> MatchGroup TcId (Located (body TcId)) -> TcM (MatchGroup Id (Located (body Id)))
zonkMatchGroup env zBody (MG { mg_alts = L l ms, mg_arg_tys = arg_tys
                             , mg_res_ty = res_ty, mg_origin = origin })
  = do  { ms' <- mapM (zonkMatch env zBody) ms
        ; arg_tys' <- zonkTcTypeToTypes env arg_tys
        ; res_ty'  <- zonkTcTypeToType env res_ty
        ; return (MG { mg_alts = L l ms', mg_arg_tys = arg_tys'
                     , mg_res_ty = res_ty', mg_origin = origin }) }

zonkMatch :: ZonkEnv
          -> (ZonkEnv -> Located (body TcId) -> TcM (Located (body Id)))
          -> LMatch TcId (Located (body TcId)) -> TcM (LMatch Id (Located (body Id)))
zonkMatch env zBody (L loc (Match mf pats _ grhss))
  = do  { (env1, new_pats) <- zonkPats env pats
        ; new_grhss <- zonkGRHSs env1 zBody grhss
        ; return (L loc (Match mf new_pats Nothing new_grhss)) }

-------------------------------------------------------------------------
zonkGRHSs :: ZonkEnv
          -> (ZonkEnv -> Located (body TcId) -> TcM (Located (body Id)))
          -> GRHSs TcId (Located (body TcId)) -> TcM (GRHSs Id (Located (body Id)))

zonkGRHSs env zBody (GRHSs grhss (L l binds)) = do
    (new_env, new_binds) <- zonkLocalBinds env binds
    let
        zonk_grhs (GRHS guarded rhs)
          = do (env2, new_guarded) <- zonkStmts new_env zonkLExpr guarded
               new_rhs <- zBody env2 rhs
               return (GRHS new_guarded new_rhs)
    new_grhss <- mapM (wrapLocM zonk_grhs) grhss
    return (GRHSs new_grhss (L l new_binds))

{-
************************************************************************
*                                                                      *
\subsection[BackSubst-HsExpr]{Running a zonkitution over a TypeCheckedExpr}
*                                                                      *
************************************************************************
-}

zonkLExprs :: ZonkEnv -> [LHsExpr TcId] -> TcM [LHsExpr Id]
zonkLExpr  :: ZonkEnv -> LHsExpr TcId   -> TcM (LHsExpr Id)
zonkExpr   :: ZonkEnv -> HsExpr TcId    -> TcM (HsExpr Id)

zonkLExprs env exprs = mapM (zonkLExpr env) exprs
zonkLExpr  env expr  = wrapLocM (zonkExpr env) expr

zonkExpr env (HsVar (L l id))
  = return (HsVar (L l (zonkIdOcc env id)))

zonkExpr _ (HsIPVar id)
  = return (HsIPVar id)

zonkExpr _ (HsOverLabel l)
  = return (HsOverLabel l)

zonkExpr env (HsLit (HsRat f ty))
  = do new_ty <- zonkTcTypeToType env ty
       return (HsLit (HsRat f new_ty))

zonkExpr _ (HsLit lit)
  = return (HsLit lit)

zonkExpr env (HsOverLit lit)
  = do  { lit' <- zonkOverLit env lit
        ; return (HsOverLit lit') }

zonkExpr env (HsLam matches)
  = do new_matches <- zonkMatchGroup env zonkLExpr matches
       return (HsLam new_matches)

zonkExpr env (HsLamCase arg matches)
  = do new_arg <- zonkTcTypeToType env arg
       new_matches <- zonkMatchGroup env zonkLExpr matches
       return (HsLamCase new_arg new_matches)

zonkExpr env (HsApp e1 e2)
  = do new_e1 <- zonkLExpr env e1
       new_e2 <- zonkLExpr env e2
       return (HsApp new_e1 new_e2)

zonkExpr _ e@(HsRnBracketOut _ _)
  = pprPanic "zonkExpr: HsRnBracketOut" (ppr e)

zonkExpr env (HsTcBracketOut body bs)
  = do bs' <- mapM zonk_b bs
       return (HsTcBracketOut body bs')
  where
    zonk_b (PendingTcSplice n e) = do e' <- zonkLExpr env e
                                      return (PendingTcSplice n e')

zonkExpr _ (HsSpliceE s) = WARN( True, ppr s ) -- Should not happen
                           return (HsSpliceE s)

zonkExpr env (OpApp e1 op fixity e2)
  = do new_e1 <- zonkLExpr env e1
       new_op <- zonkLExpr env op
       new_e2 <- zonkLExpr env e2
       return (OpApp new_e1 new_op fixity new_e2)

zonkExpr env (NegApp expr op)
  = do new_expr <- zonkLExpr env expr
       new_op <- zonkExpr env op
       return (NegApp new_expr new_op)

zonkExpr env (HsPar e)
  = do new_e <- zonkLExpr env e
       return (HsPar new_e)

zonkExpr env (SectionL expr op)
  = do new_expr <- zonkLExpr env expr
       new_op   <- zonkLExpr env op
       return (SectionL new_expr new_op)

zonkExpr env (SectionR op expr)
  = do new_op   <- zonkLExpr env op
       new_expr <- zonkLExpr env expr
       return (SectionR new_op new_expr)

zonkExpr env (ExplicitTuple tup_args boxed)
  = do { new_tup_args <- mapM zonk_tup_arg tup_args
       ; return (ExplicitTuple new_tup_args boxed) }
  where
    zonk_tup_arg (L l (Present e)) = do { e' <- zonkLExpr env e
                                        ; return (L l (Present e')) }
    zonk_tup_arg (L l (Missing t)) = do { t' <- zonkTcTypeToType env t
                                        ; return (L l (Missing t')) }

zonkExpr env (HsCase expr ms)
  = do new_expr <- zonkLExpr env expr
       new_ms <- zonkMatchGroup env zonkLExpr ms
       return (HsCase new_expr new_ms)

zonkExpr env (HsIf e0 e1 e2 e3)
  = do { new_e0 <- fmapMaybeM (zonkExpr env) e0
       ; new_e1 <- zonkLExpr env e1
       ; new_e2 <- zonkLExpr env e2
       ; new_e3 <- zonkLExpr env e3
       ; return (HsIf new_e0 new_e1 new_e2 new_e3) }

zonkExpr env (HsMultiIf ty alts)
  = do { alts' <- mapM (wrapLocM zonk_alt) alts
       ; ty'   <- zonkTcTypeToType env ty
       ; return $ HsMultiIf ty' alts' }
  where zonk_alt (GRHS guard expr)
          = do { (env', guard') <- zonkStmts env zonkLExpr guard
               ; expr'          <- zonkLExpr env' expr
               ; return $ GRHS guard' expr' }

zonkExpr env (HsLet (L l binds) expr)
  = do (new_env, new_binds) <- zonkLocalBinds env binds
       new_expr <- zonkLExpr new_env expr
       return (HsLet (L l new_binds) new_expr)

zonkExpr env (HsDo do_or_lc (L l stmts) ty)
  = do (_, new_stmts) <- zonkStmts env zonkLExpr stmts
       new_ty <- zonkTcTypeToType env ty
       return (HsDo do_or_lc (L l new_stmts) new_ty)

zonkExpr env (ExplicitList ty wit exprs)
  = do new_ty <- zonkTcTypeToType env ty
       new_wit <- zonkWit env wit
       new_exprs <- zonkLExprs env exprs
       return (ExplicitList new_ty new_wit new_exprs)
   where zonkWit _ Nothing = return Nothing
         zonkWit env (Just fln) = do new_fln <- zonkExpr env fln
                                     return (Just new_fln)

zonkExpr env (ExplicitPArr ty exprs)
  = do new_ty <- zonkTcTypeToType env ty
       new_exprs <- zonkLExprs env exprs
       return (ExplicitPArr new_ty new_exprs)

zonkExpr env expr@(RecordCon { rcon_con_expr = con_expr, rcon_flds = rbinds })
  = do  { new_con_expr <- zonkExpr env con_expr
        ; new_rbinds   <- zonkRecFields env rbinds
        ; return (expr { rcon_con_expr = new_con_expr
                       , rcon_flds = new_rbinds }) }

zonkExpr env (RecordUpd { rupd_expr = expr, rupd_flds = rbinds
                        , rupd_cons = cons, rupd_in_tys = in_tys
                        , rupd_out_tys = out_tys, rupd_wrap = req_wrap })
  = do  { new_expr    <- zonkLExpr env expr
        ; new_in_tys  <- mapM (zonkTcTypeToType env) in_tys
        ; new_out_tys <- mapM (zonkTcTypeToType env) out_tys
        ; new_rbinds  <- zonkRecUpdFields env rbinds
        ; (_, new_recwrap) <- zonkCoFn env req_wrap
        ; return (RecordUpd { rupd_expr = new_expr, rupd_flds =  new_rbinds
                            , rupd_cons = cons, rupd_in_tys = new_in_tys
                            , rupd_out_tys = new_out_tys, rupd_wrap = new_recwrap }) }

zonkExpr env (ExprWithTySigOut e ty)
  = do { e' <- zonkLExpr env e
       ; return (ExprWithTySigOut e' ty) }

zonkExpr env (ArithSeq expr wit info)
  = do new_expr <- zonkExpr env expr
       new_wit <- zonkWit env wit
       new_info <- zonkArithSeq env info
       return (ArithSeq new_expr new_wit new_info)
   where zonkWit _ Nothing = return Nothing
         zonkWit env (Just fln) = do new_fln <- zonkExpr env fln
                                     return (Just new_fln)

zonkExpr env (PArrSeq expr info)
  = do new_expr <- zonkExpr env expr
       new_info <- zonkArithSeq env info
       return (PArrSeq new_expr new_info)

zonkExpr env (HsSCC src lbl expr)
  = do new_expr <- zonkLExpr env expr
       return (HsSCC src lbl new_expr)

zonkExpr env (HsTickPragma src info expr)
  = do new_expr <- zonkLExpr env expr
       return (HsTickPragma src info new_expr)

-- hdaume: core annotations
zonkExpr env (HsCoreAnn src lbl expr)
  = do new_expr <- zonkLExpr env expr
       return (HsCoreAnn src lbl new_expr)

-- arrow notation extensions
zonkExpr env (HsProc pat body)
  = do  { (env1, new_pat) <- zonkPat env pat
        ; new_body <- zonkCmdTop env1 body
        ; return (HsProc new_pat new_body) }

-- StaticPointers extension
zonkExpr env (HsStatic expr)
  = HsStatic <$> zonkLExpr env expr

zonkExpr env (HsWrap co_fn expr)
  = do (env1, new_co_fn) <- zonkCoFn env co_fn
       new_expr <- zonkExpr env1 expr
       return (HsWrap new_co_fn new_expr)

zonkExpr _ (HsUnboundVar v)
  = return (HsUnboundVar v)

  -- nothing to do here. The payload is an LHsType, not a Type.
zonkExpr _ e@(HsTypeOut {}) = return e

zonkExpr _ expr = pprPanic "zonkExpr" (ppr expr)

-------------------------------------------------------------------------

zonkLCmd  :: ZonkEnv -> LHsCmd TcId   -> TcM (LHsCmd Id)
zonkCmd   :: ZonkEnv -> HsCmd TcId    -> TcM (HsCmd Id)

zonkLCmd  env cmd  = wrapLocM (zonkCmd env) cmd

zonkCmd env (HsCmdWrap w cmd)
  = do { (env1, w') <- zonkCoFn env w
       ; cmd' <- zonkCmd env1 cmd
       ; return (HsCmdWrap w' cmd') }
zonkCmd env (HsCmdArrApp e1 e2 ty ho rl)
  = do new_e1 <- zonkLExpr env e1
       new_e2 <- zonkLExpr env e2
       new_ty <- zonkTcTypeToType env ty
       return (HsCmdArrApp new_e1 new_e2 new_ty ho rl)

zonkCmd env (HsCmdArrForm op fixity args)
  = do new_op <- zonkLExpr env op
       new_args <- mapM (zonkCmdTop env) args
       return (HsCmdArrForm new_op fixity new_args)

zonkCmd env (HsCmdApp c e)
  = do new_c <- zonkLCmd env c
       new_e <- zonkLExpr env e
       return (HsCmdApp new_c new_e)

zonkCmd env (HsCmdLam matches)
  = do new_matches <- zonkMatchGroup env zonkLCmd matches
       return (HsCmdLam new_matches)

zonkCmd env (HsCmdPar c)
  = do new_c <- zonkLCmd env c
       return (HsCmdPar new_c)

zonkCmd env (HsCmdCase expr ms)
  = do new_expr <- zonkLExpr env expr
       new_ms <- zonkMatchGroup env zonkLCmd ms
       return (HsCmdCase new_expr new_ms)

zonkCmd env (HsCmdIf eCond ePred cThen cElse)
  = do { new_eCond <- fmapMaybeM (zonkExpr env) eCond
       ; new_ePred <- zonkLExpr env ePred
       ; new_cThen <- zonkLCmd env cThen
       ; new_cElse <- zonkLCmd env cElse
       ; return (HsCmdIf new_eCond new_ePred new_cThen new_cElse) }

zonkCmd env (HsCmdLet (L l binds) cmd)
  = do (new_env, new_binds) <- zonkLocalBinds env binds
       new_cmd <- zonkLCmd new_env cmd
       return (HsCmdLet (L l new_binds) new_cmd)

zonkCmd env (HsCmdDo (L l stmts) ty)
  = do (_, new_stmts) <- zonkStmts env zonkLCmd stmts
       new_ty <- zonkTcTypeToType env ty
       return (HsCmdDo (L l new_stmts) new_ty)





zonkCmdTop :: ZonkEnv -> LHsCmdTop TcId -> TcM (LHsCmdTop Id)
zonkCmdTop env cmd = wrapLocM (zonk_cmd_top env) cmd

zonk_cmd_top :: ZonkEnv -> HsCmdTop TcId -> TcM (HsCmdTop Id)
zonk_cmd_top env (HsCmdTop cmd stack_tys ty ids)
  = do new_cmd <- zonkLCmd env cmd
       new_stack_tys <- zonkTcTypeToType env stack_tys
       new_ty <- zonkTcTypeToType env ty
       new_ids <- mapSndM (zonkExpr env) ids
       return (HsCmdTop new_cmd new_stack_tys new_ty new_ids)

-------------------------------------------------------------------------
zonkCoFn :: ZonkEnv -> HsWrapper -> TcM (ZonkEnv, HsWrapper)
zonkCoFn env WpHole   = return (env, WpHole)
zonkCoFn env (WpCompose c1 c2) = do { (env1, c1') <- zonkCoFn env c1
                                    ; (env2, c2') <- zonkCoFn env1 c2
                                    ; return (env2, WpCompose c1' c2') }
zonkCoFn env (WpFun c1 c2 t1) = do { (env1, c1') <- zonkCoFn env c1
                                   ; (env2, c2') <- zonkCoFn env1 c2
                                   ; t1'         <- zonkTcTypeToType env2 t1
                                   ; return (env2, WpFun c1' c2' t1') }
zonkCoFn env (WpCast co) = do { co' <- zonkCoToCo env co
                              ; return (env, WpCast co') }
zonkCoFn env (WpEvLam ev)   = do { (env', ev') <- zonkEvBndrX env ev
                                 ; return (env', WpEvLam ev') }
zonkCoFn env (WpEvApp arg)  = do { arg' <- zonkEvTerm env arg
                                 ; return (env, WpEvApp arg') }
zonkCoFn env (WpTyLam tv)   = ASSERT( isImmutableTyVar tv )
                              do { (env', tv') <- zonkTyBndrX env tv
                                 ; return (env', WpTyLam tv') }
zonkCoFn env (WpTyApp ty)   = do { ty' <- zonkTcTypeToType env ty
                                 ; return (env, WpTyApp ty') }
zonkCoFn env (WpLet bs)     = do { (env1, bs') <- zonkTcEvBinds env bs
                                 ; return (env1, WpLet bs') }

-------------------------------------------------------------------------
zonkOverLit :: ZonkEnv -> HsOverLit TcId -> TcM (HsOverLit Id)
zonkOverLit env lit@(OverLit { ol_witness = e, ol_type = ty })
  = do  { ty' <- zonkTcTypeToType env ty
        ; e' <- zonkExpr env e
        ; return (lit { ol_witness = e', ol_type = ty' }) }

-------------------------------------------------------------------------
zonkArithSeq :: ZonkEnv -> ArithSeqInfo TcId -> TcM (ArithSeqInfo Id)

zonkArithSeq env (From e)
  = do new_e <- zonkLExpr env e
       return (From new_e)

zonkArithSeq env (FromThen e1 e2)
  = do new_e1 <- zonkLExpr env e1
       new_e2 <- zonkLExpr env e2
       return (FromThen new_e1 new_e2)

zonkArithSeq env (FromTo e1 e2)
  = do new_e1 <- zonkLExpr env e1
       new_e2 <- zonkLExpr env e2
       return (FromTo new_e1 new_e2)

zonkArithSeq env (FromThenTo e1 e2 e3)
  = do new_e1 <- zonkLExpr env e1
       new_e2 <- zonkLExpr env e2
       new_e3 <- zonkLExpr env e3
       return (FromThenTo new_e1 new_e2 new_e3)


-------------------------------------------------------------------------
zonkStmts :: ZonkEnv
          -> (ZonkEnv -> Located (body TcId) -> TcM (Located (body Id)))
          -> [LStmt TcId (Located (body TcId))] -> TcM (ZonkEnv, [LStmt Id (Located (body Id))])
zonkStmts env _ []     = return (env, [])
zonkStmts env zBody (s:ss) = do { (env1, s')  <- wrapLocSndM (zonkStmt env zBody) s
                                ; (env2, ss') <- zonkStmts env1 zBody ss
                                ; return (env2, s' : ss') }

zonkStmt :: ZonkEnv
         -> (ZonkEnv -> Located (body TcId) -> TcM (Located (body Id)))
         -> Stmt TcId (Located (body TcId)) -> TcM (ZonkEnv, Stmt Id (Located (body Id)))
zonkStmt env _ (ParStmt stmts_w_bndrs mzip_op bind_op)
  = do { new_stmts_w_bndrs <- mapM zonk_branch stmts_w_bndrs
       ; let new_binders = [b | ParStmtBlock _ bs _ <- new_stmts_w_bndrs, b <- bs]
             env1 = extendIdZonkEnvRec env new_binders
       ; new_mzip <- zonkExpr env1 mzip_op
       ; new_bind <- zonkExpr env1 bind_op
       ; return (env1, ParStmt new_stmts_w_bndrs new_mzip new_bind) }
  where
    zonk_branch (ParStmtBlock stmts bndrs return_op)
       = do { (env1, new_stmts) <- zonkStmts env zonkLExpr stmts
            ; new_return <- zonkExpr env1 return_op
            ; return (ParStmtBlock new_stmts (zonkIdOccs env1 bndrs) new_return) }

zonkStmt env zBody (RecStmt { recS_stmts = segStmts, recS_later_ids = lvs, recS_rec_ids = rvs
                            , recS_ret_fn = ret_id, recS_mfix_fn = mfix_id, recS_bind_fn = bind_id
                            , recS_later_rets = later_rets, recS_rec_rets = rec_rets
                            , recS_ret_ty = ret_ty })
  = do { new_rvs <- zonkIdBndrs env rvs
       ; new_lvs <- zonkIdBndrs env lvs
       ; new_ret_ty  <- zonkTcTypeToType env ret_ty
       ; new_ret_id  <- zonkExpr env ret_id
       ; new_mfix_id <- zonkExpr env mfix_id
       ; new_bind_id <- zonkExpr env bind_id
       ; let env1 = extendIdZonkEnvRec env new_rvs
       ; (env2, new_segStmts) <- zonkStmts env1 zBody segStmts
        -- Zonk the ret-expressions in an envt that
        -- has the polymorphic bindings in the envt
       ; new_later_rets <- mapM (zonkExpr env2) later_rets
       ; new_rec_rets <- mapM (zonkExpr env2) rec_rets
       ; return (extendIdZonkEnvRec env new_lvs,     -- Only the lvs are needed
                 RecStmt { recS_stmts = new_segStmts, recS_later_ids = new_lvs
                         , recS_rec_ids = new_rvs, recS_ret_fn = new_ret_id
                         , recS_mfix_fn = new_mfix_id, recS_bind_fn = new_bind_id
                         , recS_later_rets = new_later_rets
                         , recS_rec_rets = new_rec_rets, recS_ret_ty = new_ret_ty }) }

zonkStmt env zBody (BodyStmt body then_op guard_op ty)
  = do new_body <- zBody env body
       new_then <- zonkExpr env then_op
       new_guard <- zonkExpr env guard_op
       new_ty <- zonkTcTypeToType env ty
       return (env, BodyStmt new_body new_then new_guard new_ty)

zonkStmt env zBody (LastStmt body noret ret_op)
  = do new_body <- zBody env body
       new_ret <- zonkExpr env ret_op
       return (env, LastStmt new_body noret new_ret)

zonkStmt env _ (TransStmt { trS_stmts = stmts, trS_bndrs = binderMap
                              , trS_by = by, trS_form = form, trS_using = using
                              , trS_ret = return_op, trS_bind = bind_op, trS_fmap = liftM_op })
  = do { (env', stmts') <- zonkStmts env zonkLExpr stmts
    ; binderMap' <- mapM (zonkBinderMapEntry env') binderMap
    ; by'        <- fmapMaybeM (zonkLExpr env') by
    ; using'     <- zonkLExpr env using
    ; return_op' <- zonkExpr env' return_op
    ; bind_op'   <- zonkExpr env' bind_op
    ; liftM_op'  <- zonkExpr env' liftM_op
    ; let env'' = extendIdZonkEnvRec env' (map snd binderMap')
    ; return (env'', TransStmt { trS_stmts = stmts', trS_bndrs = binderMap'
                               , trS_by = by', trS_form = form, trS_using = using'
                               , trS_ret = return_op', trS_bind = bind_op', trS_fmap = liftM_op' }) }
  where
    zonkBinderMapEntry env (oldBinder, newBinder) = do
        let oldBinder' = zonkIdOcc env oldBinder
        newBinder' <- zonkIdBndr env newBinder
        return (oldBinder', newBinder')

zonkStmt env _ (LetStmt (L l binds))
  = do (env1, new_binds) <- zonkLocalBinds env binds
       return (env1, LetStmt (L l new_binds))

zonkStmt env zBody (BindStmt pat body bind_op fail_op)
  = do  { new_body <- zBody env body
        ; (env1, new_pat) <- zonkPat env pat
        ; new_bind <- zonkExpr env bind_op
        ; new_fail <- zonkExpr env fail_op
        ; return (env1, BindStmt new_pat new_body new_bind new_fail) }

zonkStmt env _zBody (ApplicativeStmt args mb_join body_ty)
  = do  { (env', args') <- zonk_args env args
        ; new_mb_join <- traverse (zonkExpr env) mb_join
        ; new_body_ty <- zonkTcTypeToType env' body_ty
        ; return (env', ApplicativeStmt args' new_mb_join new_body_ty) }
  where
   zonk_args env [] = return (env, [])
   zonk_args env ((op, arg) : groups)
      = do { (env1, arg') <- zonk_arg env arg
           ; op' <- zonkExpr env1 op
           ; (env2, ss) <- zonk_args env1 groups
           ; return (env2, (op', arg') : ss) }

   zonk_arg env (ApplicativeArgOne pat expr)
     = do { (env1, new_pat) <- zonkPat env pat
          ; new_expr <- zonkLExpr env expr
          ; return (env1, ApplicativeArgOne new_pat new_expr) }
   zonk_arg env (ApplicativeArgMany stmts ret pat)
     = do { (env1, new_stmts) <- zonkStmts env zonkLExpr stmts
          ; new_ret <- zonkExpr env1 ret
          ; (env2, new_pat) <- zonkPat env pat
          ; return (env2, ApplicativeArgMany new_stmts new_ret new_pat) }

-------------------------------------------------------------------------
zonkRecFields :: ZonkEnv -> HsRecordBinds TcId -> TcM (HsRecordBinds TcId)
zonkRecFields env (HsRecFields flds dd)
  = do  { flds' <- mapM zonk_rbind flds
        ; return (HsRecFields flds' dd) }
  where
    zonk_rbind (L l fld)
      = do { new_id   <- wrapLocM (zonkFieldOcc env) (hsRecFieldLbl fld)
           ; new_expr <- zonkLExpr env (hsRecFieldArg fld)
           ; return (L l (fld { hsRecFieldLbl = new_id
                              , hsRecFieldArg = new_expr })) }

zonkRecUpdFields :: ZonkEnv -> [LHsRecUpdField TcId] -> TcM [LHsRecUpdField TcId]
zonkRecUpdFields env = mapM zonk_rbind
  where
    zonk_rbind (L l fld)
      = do { new_id   <- wrapLocM (zonkFieldOcc env) (hsRecUpdFieldOcc fld)
           ; new_expr <- zonkLExpr env (hsRecFieldArg fld)
           ; return (L l (fld { hsRecFieldLbl = fmap ambiguousFieldOcc new_id
                              , hsRecFieldArg = new_expr })) }

-------------------------------------------------------------------------
mapIPNameTc :: (a -> TcM b) -> Either (Located HsIPName) a
            -> TcM (Either (Located HsIPName) b)
mapIPNameTc _ (Left x)  = return (Left x)
mapIPNameTc f (Right x) = do r <- f x
                             return (Right r)

{-
************************************************************************
*                                                                      *
\subsection[BackSubst-Pats]{Patterns}
*                                                                      *
************************************************************************
-}

zonkPat :: ZonkEnv -> OutPat TcId -> TcM (ZonkEnv, OutPat Id)
-- Extend the environment as we go, because it's possible for one
-- pattern to bind something that is used in another (inside or
-- to the right)
zonkPat env pat = wrapLocSndM (zonk_pat env) pat

zonk_pat :: ZonkEnv -> Pat TcId -> TcM (ZonkEnv, Pat Id)
zonk_pat env (ParPat p)
  = do  { (env', p') <- zonkPat env p
        ; return (env', ParPat p') }

zonk_pat env (WildPat ty)
  = do  { ty' <- zonkTcTypeToType env ty
        ; return (env, WildPat ty') }

zonk_pat env (VarPat (L l v))
  = do  { v' <- zonkIdBndr env v
        ; return (extendIdZonkEnv1 env v', VarPat (L l v')) }

zonk_pat env (LazyPat pat)
  = do  { (env', pat') <- zonkPat env pat
        ; return (env',  LazyPat pat') }

zonk_pat env (BangPat pat)
  = do  { (env', pat') <- zonkPat env pat
        ; return (env',  BangPat pat') }

zonk_pat env (AsPat (L loc v) pat)
  = do  { v' <- zonkIdBndr env v
        ; (env', pat') <- zonkPat (extendIdZonkEnv1 env v') pat
        ; return (env', AsPat (L loc v') pat') }

zonk_pat env (ViewPat expr pat ty)
  = do  { expr' <- zonkLExpr env expr
        ; (env', pat') <- zonkPat env pat
        ; ty' <- zonkTcTypeToType env ty
        ; return (env', ViewPat expr' pat' ty') }

zonk_pat env (ListPat pats ty Nothing)
  = do  { ty' <- zonkTcTypeToType env ty
        ; (env', pats') <- zonkPats env pats
        ; return (env', ListPat pats' ty' Nothing) }

zonk_pat env (ListPat pats ty (Just (ty2,wit)))
  = do  { wit' <- zonkExpr env wit
        ; ty2' <- zonkTcTypeToType env ty2
        ; ty' <- zonkTcTypeToType env ty
        ; (env', pats') <- zonkPats env pats
        ; return (env', ListPat pats' ty' (Just (ty2',wit'))) }

zonk_pat env (PArrPat pats ty)
  = do  { ty' <- zonkTcTypeToType env ty
        ; (env', pats') <- zonkPats env pats
        ; return (env', PArrPat pats' ty') }

zonk_pat env (TuplePat pats boxed tys)
  = do  { tys' <- mapM (zonkTcTypeToType env) tys
        ; (env', pats') <- zonkPats env pats
        ; return (env', TuplePat pats' boxed tys') }

zonk_pat env p@(ConPatOut { pat_arg_tys = tys, pat_tvs = tyvars
                          , pat_dicts = evs, pat_binds = binds
                          , pat_args = args, pat_wrap = wrapper })
  = ASSERT( all isImmutableTyVar tyvars )
    do  { new_tys <- mapM (zonkTcTypeToType env) tys
        ; (env0, new_tyvars) <- zonkTyBndrsX env tyvars
          -- Must zonk the existential variables, because their
          -- /kind/ need potential zonking.
          -- cf typecheck/should_compile/tc221.hs
        ; (env1, new_evs) <- zonkEvBndrsX env0 evs
        ; (env2, new_binds) <- zonkTcEvBinds env1 binds
        ; (env3, new_wrapper) <- zonkCoFn env2 wrapper
        ; (env', new_args) <- zonkConStuff env3 args
        ; return (env', p { pat_arg_tys = new_tys,
                            pat_tvs = new_tyvars,
                            pat_dicts = new_evs,
                            pat_binds = new_binds,
                            pat_args = new_args,
                            pat_wrap = new_wrapper}) }

zonk_pat env (LitPat lit) = return (env, LitPat lit)

zonk_pat env (SigPatOut pat ty)
  = do  { ty' <- zonkTcTypeToType env ty
        ; (env', pat') <- zonkPat env pat
        ; return (env', SigPatOut pat' ty') }

zonk_pat env (NPat (L l lit) mb_neg eq_expr)
  = do  { lit' <- zonkOverLit env lit
        ; mb_neg' <- fmapMaybeM (zonkExpr env) mb_neg
        ; eq_expr' <- zonkExpr env eq_expr
        ; return (env, NPat (L l lit') mb_neg' eq_expr') }

zonk_pat env (NPlusKPat (L loc n) (L l lit) e1 e2)
  = do  { n' <- zonkIdBndr env n
        ; lit' <- zonkOverLit env lit
        ; e1' <- zonkExpr env e1
        ; e2' <- zonkExpr env e2
        ; return (extendIdZonkEnv1 env n',
                  NPlusKPat (L loc n') (L l lit') e1' e2') }

zonk_pat env (CoPat co_fn pat ty)
  = do { (env', co_fn') <- zonkCoFn env co_fn
       ; (env'', pat') <- zonkPat env' (noLoc pat)
       ; ty' <- zonkTcTypeToType env'' ty
       ; return (env'', CoPat co_fn' (unLoc pat') ty') }

zonk_pat _ pat = pprPanic "zonk_pat" (ppr pat)

---------------------------
zonkConStuff :: ZonkEnv
             -> HsConDetails (OutPat TcId) (HsRecFields id (OutPat TcId))
             -> TcM (ZonkEnv,
                     HsConDetails (OutPat Id) (HsRecFields id (OutPat Id)))
zonkConStuff env (PrefixCon pats)
  = do  { (env', pats') <- zonkPats env pats
        ; return (env', PrefixCon pats') }

zonkConStuff env (InfixCon p1 p2)
  = do  { (env1, p1') <- zonkPat env  p1
        ; (env', p2') <- zonkPat env1 p2
        ; return (env', InfixCon p1' p2') }

zonkConStuff env (RecCon (HsRecFields rpats dd))
  = do  { (env', pats') <- zonkPats env (map (hsRecFieldArg . unLoc) rpats)
        ; let rpats' = zipWith (\(L l rp) p' -> L l (rp { hsRecFieldArg = p' }))
                               rpats pats'
        ; return (env', RecCon (HsRecFields rpats' dd)) }
        -- Field selectors have declared types; hence no zonking

---------------------------
zonkPats :: ZonkEnv -> [OutPat TcId] -> TcM (ZonkEnv, [OutPat Id])
zonkPats env []         = return (env, [])
zonkPats env (pat:pats) = do { (env1, pat') <- zonkPat env pat
                             ; (env', pats') <- zonkPats env1 pats
                             ; return (env', pat':pats') }

{-
************************************************************************
*                                                                      *
\subsection[BackSubst-Foreign]{Foreign exports}
*                                                                      *
************************************************************************
-}

zonkForeignExports :: ZonkEnv -> [LForeignDecl TcId] -> TcM [LForeignDecl Id]
zonkForeignExports env ls = mapM (wrapLocM (zonkForeignExport env)) ls

zonkForeignExport :: ZonkEnv -> ForeignDecl TcId -> TcM (ForeignDecl Id)
zonkForeignExport env (ForeignExport { fd_name = i, fd_co = co, fd_fe = spec })
  = return (ForeignExport { fd_name = fmap (zonkIdOcc env) i
                          , fd_sig_ty = undefined, fd_co = co
                          , fd_fe = spec })
zonkForeignExport _ for_imp
  = return for_imp     -- Foreign imports don't need zonking

zonkRules :: ZonkEnv -> [LRuleDecl TcId] -> TcM [LRuleDecl Id]
zonkRules env rs = mapM (wrapLocM (zonkRule env)) rs

zonkRule :: ZonkEnv -> RuleDecl TcId -> TcM (RuleDecl Id)
zonkRule env (HsRule name act (vars{-::[RuleBndr TcId]-}) lhs fv_lhs rhs fv_rhs)
  = do { unbound_tkv_set <- newMutVar emptyVarSet
       ; let kind_var_set = identify_kind_vars vars
             env_rule = setZonkType env (zonkTvCollecting kind_var_set unbound_tkv_set)
              -- See Note [Zonking the LHS of a RULE]

       ; (env_inside, new_bndrs) <- mapAccumLM zonk_bndr env_rule vars

       ; new_lhs <- zonkLExpr env_inside lhs
       ; new_rhs <- zonkLExpr env_inside rhs

       ; unbound_tkvs <- readMutVar unbound_tkv_set

       ; let final_bndrs :: [LRuleBndr Var]
             final_bndrs = map (noLoc . RuleBndr . noLoc)
                               (varSetElemsWellScoped unbound_tkvs)
                           ++ new_bndrs

       ; return $
         HsRule name act final_bndrs new_lhs fv_lhs new_rhs fv_rhs }
  where
   zonk_bndr env (L l (RuleBndr (L loc v)))
      = do { (env', v') <- zonk_it env v
           ; return (env', L l (RuleBndr (L loc v'))) }
   zonk_bndr _ (L _ (RuleBndrSig {})) = panic "zonk_bndr RuleBndrSig"

   zonk_it env v
     | isId v     = do { v' <- zonkIdBndr env v
                       ; return (extendIdZonkEnvRec env [v'], v') }
     | otherwise  = ASSERT( isImmutableTyVar v)
                    zonkTyBndrX env v
                    -- DV: used to be return (env,v) but that is plain
                    -- wrong because we may need to go inside the kind
                    -- of v and zonk there!

     -- returns the set of type variables mentioned in the kind of another
     -- type. This is used only when -XPolyKinds is not set.
   identify_kind_vars :: [LRuleBndr TcId] -> TyVarSet
   identify_kind_vars rule_bndrs
     = let vars = map strip_rulebndr rule_bndrs in
       unionVarSets (map (\v -> if isTyVar v
                                then tyCoVarsOfType (tyVarKind v)
                                else emptyVarSet) vars)

   strip_rulebndr (L _ (RuleBndr (L _ v))) = v
   strip_rulebndr (L _ (RuleBndrSig {}))   = panic "strip_rulebndr zonkRule"

zonkVects :: ZonkEnv -> [LVectDecl TcId] -> TcM [LVectDecl Id]
zonkVects env = mapM (wrapLocM (zonkVect env))

zonkVect :: ZonkEnv -> VectDecl TcId -> TcM (VectDecl Id)
zonkVect env (HsVect s v e)
  = do { v' <- wrapLocM (zonkIdBndr env) v
       ; e' <- zonkLExpr env e
       ; return $ HsVect s v' e'
       }
zonkVect env (HsNoVect s v)
  = do { v' <- wrapLocM (zonkIdBndr env) v
       ; return $ HsNoVect s v'
       }
zonkVect _env (HsVectTypeOut s t rt)
  = return $ HsVectTypeOut s t rt
zonkVect _ (HsVectTypeIn _ _ _ _) = panic "TcHsSyn.zonkVect: HsVectTypeIn"
zonkVect _env (HsVectClassOut c)
  = return $ HsVectClassOut c
zonkVect _ (HsVectClassIn _ _) = panic "TcHsSyn.zonkVect: HsVectClassIn"
zonkVect _env (HsVectInstOut i)
  = return $ HsVectInstOut i
zonkVect _ (HsVectInstIn _) = panic "TcHsSyn.zonkVect: HsVectInstIn"

{-
************************************************************************
*                                                                      *
              Constraints and evidence
*                                                                      *
************************************************************************
-}

zonkEvTerm :: ZonkEnv -> EvTerm -> TcM EvTerm
zonkEvTerm env (EvId v)           = ASSERT2( isId v, ppr v )
                                    zonkEvVarOcc env v
zonkEvTerm env (EvCoercion co)    = do { co' <- zonkCoToCo env co
                                       ; return (EvCoercion co') }
zonkEvTerm env (EvCast tm co)     = do { tm' <- zonkEvTerm env tm
                                       ; co' <- zonkCoToCo env co
                                       ; return (mkEvCast tm' co') }
zonkEvTerm _   (EvLit l)          = return (EvLit l)

zonkEvTerm env (EvTypeable ty ev) =
  do { ev' <- zonkEvTypeable env ev
     ; ty' <- zonkTcTypeToType env ty
     ; return (EvTypeable ty' ev') }
zonkEvTerm env (EvCallStack cs)
  = case cs of
      EvCsEmpty -> return (EvCallStack cs)
      EvCsPushCall n l tm -> do { tm' <- zonkEvTerm env tm
                                ; return (EvCallStack (EvCsPushCall n l tm')) }

zonkEvTerm env (EvSuperClass d n) = do { d' <- zonkEvTerm env d
                                       ; return (EvSuperClass d' n) }
zonkEvTerm env (EvDFunApp df tys tms)
  = do { tys' <- zonkTcTypeToTypes env tys
       ; tms' <- mapM (zonkEvTerm env) tms
       ; return (EvDFunApp (zonkIdOcc env df) tys' tms') }
zonkEvTerm env (EvDelayedError ty msg)
  = do { ty' <- zonkTcTypeToType env ty
       ; return (EvDelayedError ty' msg) }

zonkEvTypeable :: ZonkEnv -> EvTypeable -> TcM EvTypeable
zonkEvTypeable env (EvTypeableTyCon ts)
  = do { ts' <- mapM (zonkEvTerm env) ts
       ; return $ EvTypeableTyCon ts' }
zonkEvTypeable env (EvTypeableTyApp t1 t2)
  = do { t1' <- zonkEvTerm env t1
       ; t2' <- zonkEvTerm env t2
       ; return (EvTypeableTyApp t1' t2') }
zonkEvTypeable env (EvTypeableTyLit t1)
  = do { t1' <- zonkEvTerm env t1
       ; return (EvTypeableTyLit t1') }

zonkTcEvBinds_s :: ZonkEnv -> [TcEvBinds] -> TcM (ZonkEnv, [TcEvBinds])
zonkTcEvBinds_s env bs = do { (env, bs') <- mapAccumLM zonk_tc_ev_binds env bs
                            ; return (env, [EvBinds (unionManyBags bs')]) }

zonkTcEvBinds :: ZonkEnv -> TcEvBinds -> TcM (ZonkEnv, TcEvBinds)
zonkTcEvBinds env bs = do { (env', bs') <- zonk_tc_ev_binds env bs
                          ; return (env', EvBinds bs') }

zonk_tc_ev_binds :: ZonkEnv -> TcEvBinds -> TcM (ZonkEnv, Bag EvBind)
zonk_tc_ev_binds env (TcEvBinds var) = zonkEvBindsVar env var
zonk_tc_ev_binds env (EvBinds bs)    = zonkEvBinds env bs

zonkEvBindsVar :: ZonkEnv -> EvBindsVar -> TcM (ZonkEnv, Bag EvBind)
zonkEvBindsVar env (EvBindsVar ref _) = do { bs <- readMutVar ref
                                           ; zonkEvBinds env (evBindMapBinds bs) }

zonkEvBinds :: ZonkEnv -> Bag EvBind -> TcM (ZonkEnv, Bag EvBind)
zonkEvBinds env binds
  = {-# SCC "zonkEvBinds" #-}
    fixM (\ ~( _, new_binds) -> do
         { let env1 = extendIdZonkEnvRec env (collect_ev_bndrs new_binds)
         ; binds' <- mapBagM (zonkEvBind env1) binds
         ; return (env1, binds') })
  where
    collect_ev_bndrs :: Bag EvBind -> [EvVar]
    collect_ev_bndrs = foldrBag add []
    add (EvBind { eb_lhs = var }) vars = var : vars

zonkEvBind :: ZonkEnv -> EvBind -> TcM EvBind
zonkEvBind env bind@(EvBind { eb_lhs = var, eb_rhs = term })
  = do { var'  <- {-# SCC "zonkEvBndr" #-} zonkEvBndr env var

         -- Optimise the common case of Refl coercions
         -- See Note [Optimise coercion zonking]
         -- This has a very big effect on some programs (eg Trac #5030)

       ; term' <- case getEqPredTys_maybe (idType var') of
           Just (r, ty1, ty2) | ty1 `eqType` ty2
                  -> return (EvCoercion (mkTcReflCo r ty1))
           _other -> zonkEvTerm env term

       ; return (bind { eb_lhs = var', eb_rhs = term' }) }

{-
************************************************************************
*                                                                      *
                         Zonking types
*                                                                      *
************************************************************************

Note [Zonking the LHS of a RULE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to gather the type variables mentioned on the LHS so we can
quantify over them.  Example:
  data T a = C

  foo :: T a -> Int
  foo C = 1

  {-# RULES "myrule"  foo C = 1 #-}

After type checking the LHS becomes (foo a (C a))
and we do not want to zap the unbound tyvar 'a' to (), because
that limits the applicability of the rule.  Instead, we
want to quantify over it!

It's easiest to get zonkTvCollecting to gather the free tyvars
here. Attempts to do so earlier are tiresome, because (a) the data
type is big and (b) finding the free type vars of an expression is
necessarily monadic operation. (consider /\a -> f @ b, where b is
side-effected to a)

And that in turn is why ZonkEnv carries the function to use for
type variables!

Note [Zonking mutable unbound type or kind variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In zonkTypeZapping, we zonk mutable but unbound type or kind variables to an
arbitrary type. We know if they are unbound even though we don't carry an
environment, because at the binding site for a variable we bind the mutable
var to a fresh immutable one.  So the mutable store plays the role of an
environment.  If we come across a mutable variable that isn't so bound, it
must be completely free. We zonk the expected kind to make sure we don't get
some unbound meta variable as the kind.

Note that since we have kind polymorphism, zonk_unbound_tyvar will handle both
type and kind variables. Consider the following datatype:

  data Phantom a = Phantom Int

The type of Phantom is (forall (k : *). forall (a : k). Int). Both `a` and
`k` are unbound variables. We want to zonk this to
(forall (k : Any *). forall (a : Any (Any *)). Int).

Note [Optimise coercion zonking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When optimising evidence binds we may come across situations where
a coercion looks like
      cv = ReflCo ty
or    cv1 = cv2
where the type 'ty' is big.  In such cases it is a waste of time to zonk both
  * The variable on the LHS
  * The coercion on the RHS
Rather, we can zonk the variable, and if its type is (ty ~ ty), we can just
use Refl on the right, ignoring the actual coercion on the RHS.

This can have a very big effect, because the constraint solver sometimes does go
to a lot of effort to prove Refl!  (Eg when solving  10+3 = 10+3; cf Trac #5030)

-}

zonkTyVarOcc :: ZonkEnv -> TyVar -> TcM TcType
zonkTyVarOcc env@(ZonkEnv zonk_unbound_tyvar tv_env _) tv
  | isTcTyVar tv
  = case tcTyVarDetails tv of
         SkolemTv {}    -> lookup_in_env
         RuntimeUnk {}  -> lookup_in_env
         FlatSkol ty    -> zonkTcTypeToType env ty
         MetaTv { mtv_ref = ref }
           -> do { cts <- readMutVar ref
                 ; case cts of
                      Flexi -> do { kind <- {-# SCC "zonkKind1" #-}
                                            zonkTcTypeToType env (tyVarKind tv)
                                  ; zonk_unbound_tyvar (setTyVarKind tv kind) }
                      Indirect ty -> do { zty <- zonkTcTypeToType env ty
                                        -- Small optimisation: shortern-out indirect steps
                                        -- so that the old type may be more easily collected.
                                        ; writeMutVar ref (Indirect zty)
                                        ; return zty } }
  | otherwise
  = lookup_in_env
  where
    lookup_in_env    -- Look up in the env just as we do for Ids
      = case lookupVarEnv tv_env tv of
          Nothing  -> mkTyVarTy <$> updateTyVarKindM (zonkTcTypeToType env) tv
          Just tv' -> return (mkTyVarTy tv')

zonkCoVarOcc :: ZonkEnv -> CoVar -> TcM Coercion
zonkCoVarOcc env@(ZonkEnv _ tyco_env _) cv
  | Just cv' <- lookupVarEnv tyco_env cv  -- don't look in the knot-tied env
  = return $ mkCoVarCo cv'
  | otherwise
  = mkCoVarCo <$> updateVarTypeM (zonkTcTypeToType env) cv

zonkCoHole :: ZonkEnv -> CoercionHole
           -> Role -> Type -> Type  -- these are all redundant with
                                    -- the details in the hole,
                                    -- unzonked
           -> TcM Coercion
zonkCoHole env h r t1 t2
  = do { contents <- unpackCoercionHole_maybe h
       ; case contents of
           Just co -> do { co <- zonkCoToCo env co
                         ; checkCoercionHole co h r t1 t2 }

              -- This next case should happen only in the presence of
              -- (undeferred) type errors. Originally, I put in a panic
              -- here, but that caused too many uses of `failIfErrsM`.
           Nothing -> do { traceTc "Zonking unfilled coercion hole" (ppr h)
                         ; when debugIsOn $
                           whenNoErrs $
                           MASSERT2( False
                                   , text "Type-correct unfilled coercion hole"
                                     <+> ppr h )
                         ; t1 <- zonkTcTypeToType env t1
                         ; t2 <- zonkTcTypeToType env t2
                         ; return $ mkHoleCo h r t1 t2 } }

zonk_tycomapper :: TyCoMapper ZonkEnv TcM
zonk_tycomapper = TyCoMapper
  { tcm_smart = True   -- Establish type invariants
                       -- See Note [Type-checking inside the knot] in TcHsType
  , tcm_tyvar = zonkTyVarOcc
  , tcm_covar = zonkCoVarOcc
  , tcm_hole  = zonkCoHole
  , tcm_tybinder = \env tv _vis -> zonkTyBndrX env tv }

zonkTcTypeToType :: ZonkEnv -> TcType -> TcM Type
zonkTcTypeToType = mapType zonk_tycomapper

zonkTcTypeToTypes :: ZonkEnv -> [TcType] -> TcM [Type]
zonkTcTypeToTypes env tys = mapM (zonkTcTypeToType env) tys

zonkCoToCo :: ZonkEnv -> Coercion -> TcM Coercion
zonkCoToCo = mapCoercion zonk_tycomapper

zonkTvCollecting :: TyVarSet -> TcRef TyVarSet -> UnboundTyVarZonker
-- This variant collects unbound type variables in a mutable variable
-- Works on both types and kinds
zonkTvCollecting kind_vars unbound_tv_set tv
  = do { poly_kinds <- xoptM LangExt.PolyKinds
       ; if tv `elemVarSet` kind_vars && not poly_kinds then defaultKindVar tv else do
       { ty_or_tv <- zonkQuantifiedTyVarOrType tv
       ; case ty_or_tv of
           Right ty -> return ty
           Left tv' -> do
             { tv_set <- readMutVar unbound_tv_set
             ; writeMutVar unbound_tv_set (extendVarSet tv_set tv')
             ; return (mkTyVarTy tv') } } }

zonkTypeZapping :: UnboundTyVarZonker
-- This variant is used for everything except the LHS of rules
-- It zaps unbound type variables to (), or some other arbitrary type
-- Works on both types and kinds
zonkTypeZapping tv
  = do { let ty | isLevityVar tv = liftedDataConTy
                | otherwise      = anyTypeOfKind (tyVarKind tv)
       ; writeMetaTyVar tv ty
       ; return ty }
