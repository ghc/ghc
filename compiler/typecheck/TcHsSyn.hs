{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1996-1998


TcHsSyn: Specialisations of the @HsSyn@ syntax for the typechecker

This module is an extension of @HsSyn@ syntax, for use in the type
checker.
-}

{-# LANGUAGE CPP, TupleSections #-}
{-# LANGUAGE CPP, TypeFamilies #-}

module TcHsSyn (
        -- * Extracting types from HsSyn
        hsLitType, hsLPatType, hsPatType,

        -- * Other HsSyn functions
        mkHsDictLet, mkHsApp,
        mkHsAppTy, mkHsCaseAlt,
        shortCutLit, hsOverLitName,
        conLikeResTy,

        -- * re-exported from TcMonad
        TcId, TcIdSet,

        -- * Zonking
        -- | For a description of "zonking", see Note [What is zonking?]
        -- in TcMType
        zonkTopDecls, zonkTopExpr, zonkTopLExpr,
        zonkTopBndrs, zonkTyBndrsX,
        zonkTyVarBindersX, zonkTyVarBinderX,
        emptyZonkEnv, mkEmptyZonkEnv,
        zonkTcTypeToType, zonkTcTypeToTypes, zonkTyVarOcc,
        zonkCoToCo, zonkSigType,
        zonkEvBinds, zonkTcEvBinds
  ) where

#include "HsVersions.h"

import HsSyn
import Id
import IdInfo
import TcRnMonad
import PrelNames
import TcType
import TcMType
import TcEvidence
import TysPrim
import TyCon   ( isUnboxedTupleTyCon )
import TysWiredIn
import Type
import Coercion
import ConLike
import DataCon
import HscTypes
import Name
import NameEnv
import Var
import VarEnv
import DynFlags
import Literal
import BasicTypes
import Maybes
import SrcLoc
import Bag
import Outputable
import Util
import UniqFM

import Control.Monad
import Data.List  ( partition )
import Control.Arrow ( second )

{-
************************************************************************
*                                                                      *
       Extracting the type from HsSyn
*                                                                      *
************************************************************************

-}

hsLPatType :: OutPat GhcTc -> Type
hsLPatType (L _ pat) = hsPatType pat

hsPatType :: Pat GhcTc -> Type
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
hsPatType (SumPat _ _ _ tys)          = mkSumTy tys
hsPatType (ConPatOut { pat_con = L _ con, pat_arg_tys = tys })
                                      = conLikeResTy con tys
hsPatType (SigPatOut _ ty)            = ty
hsPatType (NPat _ _ _ ty)             = ty
hsPatType (NPlusKPat _ _ _ _ _ ty)    = ty
hsPatType (CoPat _ _ ty)              = ty
hsPatType p                           = pprPanic "hsPatType" (ppr p)

hsLitType :: HsLit p -> TcType
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
hsLitType (HsRat _ _ ty)     = ty
hsLitType (HsFloatPrim _ _)  = floatPrimTy
hsLitType (HsDoublePrim _ _) = doublePrimTy

-- Overloaded literals. Here mainly because it uses isIntTy etc

shortCutLit :: DynFlags -> OverLitVal -> TcType -> Maybe (HsExpr GhcTcId)
shortCutLit dflags (HsIntegral int@(IL src neg i)) ty
  | isIntTy ty  && inIntRange  dflags i = Just (HsLit (HsInt def int))
  | isWordTy ty && inWordRange dflags i = Just (mkLit wordDataCon (HsWordPrim src i))
  | isIntegerTy ty = Just (HsLit (HsInteger src i ty))
  | otherwise = shortCutLit dflags (HsFractional (integralFractionalLit neg i)) ty
        -- The 'otherwise' case is important
        -- Consider (3 :: Float).  Syntactically it looks like an IntLit,
        -- so we'll call shortCutIntLit, but of course it's a float
        -- This can make a big difference for programs with a lot of
        -- literals, compiled without -O

shortCutLit _ (HsFractional f) ty
  | isFloatTy ty  = Just (mkLit floatDataCon  (HsFloatPrim def f))
  | isDoubleTy ty = Just (mkLit doubleDataCon (HsDoublePrim def f))
  | otherwise     = Nothing

shortCutLit _ (HsIsString src s) ty
  | isStringTy ty = Just (HsLit (HsString src s))
  | otherwise     = Nothing

mkLit :: DataCon -> HsLit GhcTc -> HsExpr GhcTc
mkLit con lit = HsApp (nlHsDataCon con) (nlHsLit lit)

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

-- Confused by zonking? See Note [What is zonking?] in TcMType.
type UnboundTyVarZonker = TcTyVar -> TcM Type
        -- How to zonk an unbound type variable
        -- The TcTyVar is
        --     (a) a MetaTv
        --     (b) Flexi and
        --     (c) its kind is already zonked
        -- Note [Zonking the LHS of a RULE]

-- | A ZonkEnv carries around several bits.
-- The UnboundTyVarZonker just zaps unbouned meta-tyvars to Any (as
-- defined in zonkTypeZapping), except on the LHS of rules. See
-- Note [Zonking the LHS of a RULE].
--
-- The (TyCoVarEnv TyVar) and is just an optimisation: when binding a
-- tyvar or covar, we zonk the kind right away and add a mapping to
-- the env. This prevents re-zonking the kind at every occurrence. But
-- this is *just* an optimisation.
--
-- The final (IdEnv Var) optimises zonking for Ids. It is
-- knot-tied. We must be careful never to put coercion variables
-- (which are Ids, after all) in the knot-tied env, because coercions
-- can appear in types, and we sometimes inspect a zonked type in this
-- module.
--
-- Confused by zonking? See Note [What is zonking?] in TcMType.
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
  ppr (ZonkEnv _ _ty_env var_env) = pprUFM var_env (vcat . map ppr)


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
extendTyZonkEnv1 (ZonkEnv zonk_ty ty_env id_env) tv
  = ZonkEnv zonk_ty (extendVarEnv ty_env tv tv) id_env

setZonkType :: ZonkEnv -> UnboundTyVarZonker -> ZonkEnv
setZonkType (ZonkEnv _ ty_env id_env) zonk_ty
  = ZonkEnv zonk_ty ty_env id_env

zonkEnvIds :: ZonkEnv -> TypeEnv
zonkEnvIds (ZonkEnv _ _ id_env) =
  mkNameEnv [(getName id, AnId id) | id <- nonDetEltsUFM id_env]
  -- It's OK to use nonDetEltsUFM here because we forget the ordering
  -- immediately by creating a TypeEnv

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
zonkIdBndr env v
  = do ty' <- zonkTcTypeToType env (idType v)
       ensureNotLevPoly ty'
         (text "In the type of binder" <+> quotes (ppr v))

       return (modifyIdInfo (`setLevityInfoWithType` ty') (setIdType v ty'))

zonkIdBndrs :: ZonkEnv -> [TcId] -> TcM [Id]
zonkIdBndrs env ids = mapM (zonkIdBndr env) ids

zonkTopBndrs :: [TcId] -> TcM [Id]
zonkTopBndrs ids = zonkIdBndrs emptyZonkEnv ids

zonkFieldOcc :: ZonkEnv -> FieldOcc GhcTcId -> TcM (FieldOcc GhcTc)
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

zonkTyBndrsX :: ZonkEnv -> [TcTyVar] -> TcM (ZonkEnv, [TyVar])
zonkTyBndrsX = mapAccumLM zonkTyBndrX

zonkTyBndrX :: ZonkEnv -> TcTyVar -> TcM (ZonkEnv, TyVar)
-- This guarantees to return a TyVar (not a TcTyVar)
-- then we add it to the envt, so all occurrences are replaced
zonkTyBndrX env tv
  = ASSERT( isImmutableTyVar tv )
    do { ki <- zonkTcTypeToType env (tyVarKind tv)
               -- Internal names tidy up better, for iface files.
       ; let tv' = mkTyVar (tyVarName tv) ki
       ; return (extendTyZonkEnv1 env tv', tv') }

zonkTyVarBindersX :: ZonkEnv -> [TyVarBndr TcTyVar vis]
                             -> TcM (ZonkEnv, [TyVarBndr TyVar vis])
zonkTyVarBindersX = mapAccumLM zonkTyVarBinderX

zonkTyVarBinderX :: ZonkEnv -> TyVarBndr TcTyVar vis
                            -> TcM (ZonkEnv, TyVarBndr TyVar vis)
-- Takes a TcTyVar and guarantees to return a TyVar
zonkTyVarBinderX env (TvBndr tv vis)
  = do { (env', tv') <- zonkTyBndrX env tv
       ; return (env', TvBndr tv' vis) }

zonkTopExpr :: HsExpr GhcTcId -> TcM (HsExpr GhcTc)
zonkTopExpr e = zonkExpr emptyZonkEnv e

zonkTopLExpr :: LHsExpr GhcTcId -> TcM (LHsExpr GhcTc)
zonkTopLExpr e = zonkLExpr emptyZonkEnv e

zonkTopDecls :: Bag EvBind
             -> LHsBinds GhcTcId
             -> [LRuleDecl GhcTcId] -> [LVectDecl GhcTcId] -> [LTcSpecPrag]
             -> [LForeignDecl GhcTcId]
             -> TcM (TypeEnv,
                     Bag EvBind,
                     LHsBinds GhcTc,
                     [LForeignDecl GhcTc],
                     [LTcSpecPrag],
                     [LRuleDecl    GhcTc],
                     [LVectDecl    GhcTc])
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
zonkLocalBinds :: ZonkEnv -> HsLocalBinds GhcTcId
               -> TcM (ZonkEnv, HsLocalBinds GhcTc)
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
zonkRecMonoBinds :: ZonkEnv -> LHsBinds GhcTcId -> TcM (ZonkEnv, LHsBinds GhcTc)
zonkRecMonoBinds env binds
 = fixM (\ ~(_, new_binds) -> do
        { let env1 = extendIdZonkEnvRec env (collectHsBindsBinders new_binds)
        ; binds' <- zonkMonoBinds env1 binds
        ; return (env1, binds') })

---------------------------------------------
zonkMonoBinds :: ZonkEnv -> LHsBinds GhcTcId -> TcM (LHsBinds GhcTc)
zonkMonoBinds env binds = mapBagM (zonk_lbind env) binds

zonk_lbind :: ZonkEnv -> LHsBind GhcTcId -> TcM (LHsBind GhcTc)
zonk_lbind env = wrapLocM (zonk_bind env)

zonk_bind :: ZonkEnv -> HsBind GhcTcId -> TcM (HsBind GhcTc)
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
    zonkExport env (ABE{ abe_wrap = wrap
                       , abe_poly = poly_id
                       , abe_mono = mono_id, abe_prags = prags })
        = do new_poly_id <- zonkIdBndr env poly_id
             (_, new_wrap) <- zonkCoFn env wrap
             new_prags <- zonkSpecPrags env prags
             return (ABE{ abe_wrap = new_wrap
                        , abe_poly = new_poly_id
                        , abe_mono = zonkIdOcc env mono_id
                        , abe_prags = new_prags })

zonk_bind env outer_bind@(AbsBindsSig { abs_tvs         = tyvars
                                      , abs_ev_vars     = evs
                                      , abs_sig_export  = poly
                                      , abs_sig_prags   = prags
                                      , abs_sig_ev_bind = ev_bind
                                      , abs_sig_bind    = lbind })
  | L bind_loc bind@(FunBind { fun_id      = L loc local
                             , fun_matches = ms
                             , fun_co_fn   = co_fn }) <- lbind
  = ASSERT( all isImmutableTyVar tyvars )
    do { (env0, new_tyvars)  <- zonkTyBndrsX env  tyvars
       ; (env1, new_evs)     <- zonkEvBndrsX env0 evs
       ; (env2, new_ev_bind) <- zonkTcEvBinds env1 ev_bind
           -- Inline zonk_bind (FunBind ...) because we wish to skip
           -- the check for representation-polymorphic binders. The
           -- local binder in the FunBind in an AbsBindsSig is never actually
           -- bound in Core -- indeed, that's the whole point of AbsBindsSig.
           -- just calling zonk_bind causes #11405.
       ; new_local           <- updateVarTypeM (zonkTcTypeToType env2) local
       ; (env3, new_co_fn)   <- zonkCoFn env2 co_fn
       ; new_ms              <- zonkMatchGroup env3 zonkLExpr ms
           -- If there is a representation polymorphism problem, it will
           -- be caught here:
       ; new_poly_id         <- zonkIdBndr env2 poly
       ; new_prags           <- zonkSpecPrags env2 prags
       ; let new_val_bind = L bind_loc (bind { fun_id      = L loc new_local
                                             , fun_matches = new_ms
                                             , fun_co_fn   = new_co_fn })
       ; return (AbsBindsSig { abs_tvs         = new_tyvars
                             , abs_ev_vars     = new_evs
                             , abs_sig_export  = new_poly_id
                             , abs_sig_prags   = new_prags
                             , abs_sig_ev_bind = new_ev_bind
                             , abs_sig_bind    = new_val_bind  }) }

  | otherwise
  = pprPanic "zonk_bind" (ppr outer_bind)

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

zonkPatSynDir :: ZonkEnv -> HsPatSynDir GhcTcId
              -> TcM (ZonkEnv, HsPatSynDir GhcTc)
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
            -> (ZonkEnv -> Located (body GhcTcId) -> TcM (Located (body GhcTc)))
            -> MatchGroup GhcTcId (Located (body GhcTcId))
            -> TcM (MatchGroup GhcTc (Located (body GhcTc)))
zonkMatchGroup env zBody (MG { mg_alts = L l ms, mg_arg_tys = arg_tys
                             , mg_res_ty = res_ty, mg_origin = origin })
  = do  { ms' <- mapM (zonkMatch env zBody) ms
        ; arg_tys' <- zonkTcTypeToTypes env arg_tys
        ; res_ty'  <- zonkTcTypeToType env res_ty
        ; return (MG { mg_alts = L l ms', mg_arg_tys = arg_tys'
                     , mg_res_ty = res_ty', mg_origin = origin }) }

zonkMatch :: ZonkEnv
          -> (ZonkEnv -> Located (body GhcTcId) -> TcM (Located (body GhcTc)))
          -> LMatch GhcTcId (Located (body GhcTcId))
          -> TcM (LMatch GhcTc (Located (body GhcTc)))
zonkMatch env zBody (L loc (Match mf pats _ grhss))
  = do  { (env1, new_pats) <- zonkPats env pats
        ; new_grhss <- zonkGRHSs env1 zBody grhss
        ; return (L loc (Match mf new_pats Nothing new_grhss)) }

-------------------------------------------------------------------------
zonkGRHSs :: ZonkEnv
          -> (ZonkEnv -> Located (body GhcTcId) -> TcM (Located (body GhcTc)))
          -> GRHSs GhcTcId (Located (body GhcTcId))
          -> TcM (GRHSs GhcTc (Located (body GhcTc)))

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

zonkLExprs :: ZonkEnv -> [LHsExpr GhcTcId] -> TcM [LHsExpr GhcTc]
zonkLExpr  :: ZonkEnv -> LHsExpr GhcTcId   -> TcM (LHsExpr GhcTc)
zonkExpr   :: ZonkEnv -> HsExpr GhcTcId    -> TcM (HsExpr GhcTc)

zonkLExprs env exprs = mapM (zonkLExpr env) exprs
zonkLExpr  env expr  = wrapLocM (zonkExpr env) expr

zonkExpr env (HsVar (L l id))
  = ASSERT2( isNothing (isDataConId_maybe id), ppr id )
    return (HsVar (L l (zonkIdOcc env id)))

zonkExpr _ e@(HsConLikeOut {}) = return e

zonkExpr _ (HsIPVar id)
  = return (HsIPVar id)

zonkExpr _ e@HsOverLabel{} = return e

zonkExpr env (HsLit (HsRat e f ty))
  = do new_ty <- zonkTcTypeToType env ty
       return (HsLit (HsRat e f new_ty))

zonkExpr _ (HsLit lit)
  = return (HsLit lit)

zonkExpr env (HsOverLit lit)
  = do  { lit' <- zonkOverLit env lit
        ; return (HsOverLit lit') }

zonkExpr env (HsLam matches)
  = do new_matches <- zonkMatchGroup env zonkLExpr matches
       return (HsLam new_matches)

zonkExpr env (HsLamCase matches)
  = do new_matches <- zonkMatchGroup env zonkLExpr matches
       return (HsLamCase new_matches)

zonkExpr env (HsApp e1 e2)
  = do new_e1 <- zonkLExpr env e1
       new_e2 <- zonkLExpr env e2
       return (HsApp new_e1 new_e2)

zonkExpr env (HsAppTypeOut e t)
  = do new_e <- zonkLExpr env e
       return (HsAppTypeOut new_e t)
       -- NB: the type is an HsType; can't zonk that!

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
  = do (env', new_op) <- zonkSyntaxExpr env op
       new_expr <- zonkLExpr env' expr
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

zonkExpr env (ExplicitSum alt arity expr args)
  = do new_args <- mapM (zonkTcTypeToType env) args
       new_expr <- zonkLExpr env expr
       return (ExplicitSum alt arity new_expr new_args)

zonkExpr env (HsCase expr ms)
  = do new_expr <- zonkLExpr env expr
       new_ms <- zonkMatchGroup env zonkLExpr ms
       return (HsCase new_expr new_ms)

zonkExpr env (HsIf Nothing e1 e2 e3)
  = do new_e1 <- zonkLExpr env e1
       new_e2 <- zonkLExpr env e2
       new_e3 <- zonkLExpr env e3
       return (HsIf Nothing new_e1 new_e2 new_e3)

zonkExpr env (HsIf (Just fun) e1 e2 e3)
  = do (env1, new_fun) <- zonkSyntaxExpr env fun
       new_e1 <- zonkLExpr env1 e1
       new_e2 <- zonkLExpr env1 e2
       new_e3 <- zonkLExpr env1 e3
       return (HsIf (Just new_fun) new_e1 new_e2 new_e3)

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
  = do (env1, new_wit) <- zonkWit env wit
       new_ty <- zonkTcTypeToType env1 ty
       new_exprs <- zonkLExprs env1 exprs
       return (ExplicitList new_ty new_wit new_exprs)
   where zonkWit env Nothing    = return (env, Nothing)
         zonkWit env (Just fln) = second Just <$> zonkSyntaxExpr env fln

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
  = do (env1, new_wit) <- zonkWit env wit
       new_expr <- zonkExpr env expr
       new_info <- zonkArithSeq env1 info
       return (ArithSeq new_expr new_wit new_info)
   where zonkWit env Nothing    = return (env, Nothing)
         zonkWit env (Just fln) = second Just <$> zonkSyntaxExpr env fln

zonkExpr env (PArrSeq expr info)
  = do new_expr <- zonkExpr env expr
       new_info <- zonkArithSeq env info
       return (PArrSeq new_expr new_info)

zonkExpr env (HsSCC src lbl expr)
  = do new_expr <- zonkLExpr env expr
       return (HsSCC src lbl new_expr)

zonkExpr env (HsTickPragma src info srcInfo expr)
  = do new_expr <- zonkLExpr env expr
       return (HsTickPragma src info srcInfo new_expr)

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
zonkExpr env (HsStatic fvs expr)
  = HsStatic fvs <$> zonkLExpr env expr

zonkExpr env (HsWrap co_fn expr)
  = do (env1, new_co_fn) <- zonkCoFn env co_fn
       new_expr <- zonkExpr env1 expr
       return (HsWrap new_co_fn new_expr)

zonkExpr _ e@(HsUnboundVar {}) = return e

zonkExpr _ expr = pprPanic "zonkExpr" (ppr expr)

-------------------------------------------------------------------------
{-
Note [Skolems in zonkSyntaxExpr]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider rebindable syntax with something like

  (>>=) :: (forall x. blah) -> (forall y. blah') -> blah''

The x and y become skolems that are in scope when type-checking the
arguments to the bind. This means that we must extend the ZonkEnv with
these skolems when zonking the arguments to the bind. But the skolems
are different between the two arguments, and so we should theoretically
carry around different environments to use for the different arguments.

However, this becomes a logistical nightmare, especially in dealing with
the more exotic Stmt forms. So, we simplify by making the critical
assumption that the uniques of the skolems are different. (This assumption
is justified by the use of newUnique in TcMType.instSkolTyCoVarX.)
Now, we can safely just extend one environment.
-}

-- See Note [Skolems in zonkSyntaxExpr]
zonkSyntaxExpr :: ZonkEnv -> SyntaxExpr GhcTcId
               -> TcM (ZonkEnv, SyntaxExpr GhcTc)
zonkSyntaxExpr env (SyntaxExpr { syn_expr      = expr
                               , syn_arg_wraps = arg_wraps
                               , syn_res_wrap  = res_wrap })
  = do { (env0, res_wrap')  <- zonkCoFn env res_wrap
       ; expr'              <- zonkExpr env0 expr
       ; (env1, arg_wraps') <- mapAccumLM zonkCoFn env0 arg_wraps
       ; return (env1, SyntaxExpr { syn_expr      = expr'
                                  , syn_arg_wraps = arg_wraps'
                                  , syn_res_wrap  = res_wrap' }) }

-------------------------------------------------------------------------

zonkLCmd  :: ZonkEnv -> LHsCmd GhcTcId   -> TcM (LHsCmd GhcTc)
zonkCmd   :: ZonkEnv -> HsCmd GhcTcId    -> TcM (HsCmd GhcTc)

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

zonkCmd env (HsCmdArrForm op f fixity args)
  = do new_op <- zonkLExpr env op
       new_args <- mapM (zonkCmdTop env) args
       return (HsCmdArrForm new_op f fixity new_args)

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
  = do { (env1, new_eCond) <- zonkWit env eCond
       ; new_ePred <- zonkLExpr env1 ePred
       ; new_cThen <- zonkLCmd env1 cThen
       ; new_cElse <- zonkLCmd env1 cElse
       ; return (HsCmdIf new_eCond new_ePred new_cThen new_cElse) }
  where
    zonkWit env Nothing  = return (env, Nothing)
    zonkWit env (Just w) = second Just <$> zonkSyntaxExpr env w

zonkCmd env (HsCmdLet (L l binds) cmd)
  = do (new_env, new_binds) <- zonkLocalBinds env binds
       new_cmd <- zonkLCmd new_env cmd
       return (HsCmdLet (L l new_binds) new_cmd)

zonkCmd env (HsCmdDo (L l stmts) ty)
  = do (_, new_stmts) <- zonkStmts env zonkLCmd stmts
       new_ty <- zonkTcTypeToType env ty
       return (HsCmdDo (L l new_stmts) new_ty)





zonkCmdTop :: ZonkEnv -> LHsCmdTop GhcTcId -> TcM (LHsCmdTop GhcTc)
zonkCmdTop env cmd = wrapLocM (zonk_cmd_top env) cmd

zonk_cmd_top :: ZonkEnv -> HsCmdTop GhcTcId -> TcM (HsCmdTop GhcTc)
zonk_cmd_top env (HsCmdTop cmd stack_tys ty ids)
  = do new_cmd <- zonkLCmd env cmd
       new_stack_tys <- zonkTcTypeToType env stack_tys
       new_ty <- zonkTcTypeToType env ty
       new_ids <- mapSndM (zonkExpr env) ids

       MASSERT( isLiftedTypeKind (typeKind new_stack_tys) )
         -- desugarer assumes that this is not levity polymorphic...
         -- but indeed it should always be lifted due to the typing
         -- rules for arrows

       return (HsCmdTop new_cmd new_stack_tys new_ty new_ids)

-------------------------------------------------------------------------
zonkCoFn :: ZonkEnv -> HsWrapper -> TcM (ZonkEnv, HsWrapper)
zonkCoFn env WpHole   = return (env, WpHole)
zonkCoFn env (WpCompose c1 c2) = do { (env1, c1') <- zonkCoFn env c1
                                    ; (env2, c2') <- zonkCoFn env1 c2
                                    ; return (env2, WpCompose c1' c2') }
zonkCoFn env (WpFun c1 c2 t1 d) = do { (env1, c1') <- zonkCoFn env c1
                                     ; (env2, c2') <- zonkCoFn env1 c2
                                     ; t1'         <- zonkTcTypeToType env2 t1
                                     ; return (env2, WpFun c1' c2' t1' d) }
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
zonkOverLit :: ZonkEnv -> HsOverLit GhcTcId -> TcM (HsOverLit GhcTc)
zonkOverLit env lit@(OverLit { ol_witness = e, ol_type = ty })
  = do  { ty' <- zonkTcTypeToType env ty
        ; e' <- zonkExpr env e
        ; return (lit { ol_witness = e', ol_type = ty' }) }

-------------------------------------------------------------------------
zonkArithSeq :: ZonkEnv -> ArithSeqInfo GhcTcId -> TcM (ArithSeqInfo GhcTc)

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
          -> (ZonkEnv -> Located (body GhcTcId) -> TcM (Located (body GhcTc)))
          -> [LStmt GhcTcId (Located (body GhcTcId))]
          -> TcM (ZonkEnv, [LStmt GhcTc (Located (body GhcTc))])
zonkStmts env _ []     = return (env, [])
zonkStmts env zBody (s:ss) = do { (env1, s')  <- wrapLocSndM (zonkStmt env zBody) s
                                ; (env2, ss') <- zonkStmts env1 zBody ss
                                ; return (env2, s' : ss') }

zonkStmt :: ZonkEnv
         -> (ZonkEnv -> Located (body GhcTcId) -> TcM (Located (body GhcTc)))
         -> Stmt GhcTcId (Located (body GhcTcId))
         -> TcM (ZonkEnv, Stmt GhcTc (Located (body GhcTc)))
zonkStmt env _ (ParStmt stmts_w_bndrs mzip_op bind_op bind_ty)
  = do { (env1, new_bind_op) <- zonkSyntaxExpr env bind_op
       ; new_bind_ty <- zonkTcTypeToType env1 bind_ty
       ; new_stmts_w_bndrs <- mapM (zonk_branch env1) stmts_w_bndrs
       ; let new_binders = [b | ParStmtBlock _ bs _ <- new_stmts_w_bndrs, b <- bs]
             env2 = extendIdZonkEnvRec env1 new_binders
       ; new_mzip <- zonkExpr env2 mzip_op
       ; return (env2, ParStmt new_stmts_w_bndrs new_mzip new_bind_op new_bind_ty) }
  where
    zonk_branch env1 (ParStmtBlock stmts bndrs return_op)
       = do { (env2, new_stmts)  <- zonkStmts env1 zonkLExpr stmts
            ; (env3, new_return) <- zonkSyntaxExpr env2 return_op
            ; return (ParStmtBlock new_stmts (zonkIdOccs env3 bndrs) new_return) }

zonkStmt env zBody (RecStmt { recS_stmts = segStmts, recS_later_ids = lvs, recS_rec_ids = rvs
                            , recS_ret_fn = ret_id, recS_mfix_fn = mfix_id
                            , recS_bind_fn = bind_id, recS_bind_ty = bind_ty
                            , recS_later_rets = later_rets, recS_rec_rets = rec_rets
                            , recS_ret_ty = ret_ty })
  = do { (env1, new_bind_id) <- zonkSyntaxExpr env bind_id
       ; (env2, new_mfix_id) <- zonkSyntaxExpr env1 mfix_id
       ; (env3, new_ret_id)  <- zonkSyntaxExpr env2 ret_id
       ; new_bind_ty <- zonkTcTypeToType env3 bind_ty
       ; new_rvs <- zonkIdBndrs env3 rvs
       ; new_lvs <- zonkIdBndrs env3 lvs
       ; new_ret_ty  <- zonkTcTypeToType env3 ret_ty
       ; let env4 = extendIdZonkEnvRec env3 new_rvs
       ; (env5, new_segStmts) <- zonkStmts env4 zBody segStmts
        -- Zonk the ret-expressions in an envt that
        -- has the polymorphic bindings in the envt
       ; new_later_rets <- mapM (zonkExpr env5) later_rets
       ; new_rec_rets <- mapM (zonkExpr env5) rec_rets
       ; return (extendIdZonkEnvRec env3 new_lvs,     -- Only the lvs are needed
                 RecStmt { recS_stmts = new_segStmts, recS_later_ids = new_lvs
                         , recS_rec_ids = new_rvs, recS_ret_fn = new_ret_id
                         , recS_mfix_fn = new_mfix_id, recS_bind_fn = new_bind_id
                         , recS_bind_ty = new_bind_ty
                         , recS_later_rets = new_later_rets
                         , recS_rec_rets = new_rec_rets, recS_ret_ty = new_ret_ty }) }

zonkStmt env zBody (BodyStmt body then_op guard_op ty)
  = do (env1, new_then_op)  <- zonkSyntaxExpr env then_op
       (env2, new_guard_op) <- zonkSyntaxExpr env1 guard_op
       new_body <- zBody env2 body
       new_ty   <- zonkTcTypeToType env2 ty
       return (env2, BodyStmt new_body new_then_op new_guard_op new_ty)

zonkStmt env zBody (LastStmt body noret ret_op)
  = do (env1, new_ret) <- zonkSyntaxExpr env ret_op
       new_body <- zBody env1 body
       return (env, LastStmt new_body noret new_ret)

zonkStmt env _ (TransStmt { trS_stmts = stmts, trS_bndrs = binderMap
                          , trS_by = by, trS_form = form, trS_using = using
                          , trS_ret = return_op, trS_bind = bind_op
                          , trS_bind_arg_ty = bind_arg_ty
                          , trS_fmap = liftM_op })
  = do {
    ; (env1, bind_op') <- zonkSyntaxExpr env bind_op
    ; bind_arg_ty' <- zonkTcTypeToType env1 bind_arg_ty
    ; (env2, stmts') <- zonkStmts env1 zonkLExpr stmts
    ; by'        <- fmapMaybeM (zonkLExpr env2) by
    ; using'     <- zonkLExpr env2 using

    ; (env3, return_op') <- zonkSyntaxExpr env2 return_op
    ; binderMap' <- mapM (zonkBinderMapEntry env3) binderMap
    ; liftM_op'  <- zonkExpr env3 liftM_op
    ; let env3' = extendIdZonkEnvRec env3 (map snd binderMap')
    ; return (env3', TransStmt { trS_stmts = stmts', trS_bndrs = binderMap'
                               , trS_by = by', trS_form = form, trS_using = using'
                               , trS_ret = return_op', trS_bind = bind_op'
                               , trS_bind_arg_ty = bind_arg_ty'
                               , trS_fmap = liftM_op' }) }
  where
    zonkBinderMapEntry env  (oldBinder, newBinder) = do
        let oldBinder' = zonkIdOcc env oldBinder
        newBinder' <- zonkIdBndr env newBinder
        return (oldBinder', newBinder')

zonkStmt env _ (LetStmt (L l binds))
  = do (env1, new_binds) <- zonkLocalBinds env binds
       return (env1, LetStmt (L l new_binds))

zonkStmt env zBody (BindStmt pat body bind_op fail_op bind_ty)
  = do  { (env1, new_bind) <- zonkSyntaxExpr env bind_op
        ; new_bind_ty <- zonkTcTypeToType env1 bind_ty
        ; new_body <- zBody env1 body
        ; (env2, new_pat) <- zonkPat env1 pat
        ; (_, new_fail) <- zonkSyntaxExpr env1 fail_op
        ; return (env2, BindStmt new_pat new_body new_bind new_fail new_bind_ty) }

-- Scopes: join > ops (in reverse order) > pats (in forward order)
--              > rest of stmts
zonkStmt env _zBody (ApplicativeStmt args mb_join body_ty)
  = do  { (env1, new_mb_join)   <- zonk_join env mb_join
        ; (env2, new_args)      <- zonk_args env1 args
        ; new_body_ty           <- zonkTcTypeToType env2 body_ty
        ; return (env2, ApplicativeStmt new_args new_mb_join new_body_ty) }
  where
    zonk_join env Nothing  = return (env, Nothing)
    zonk_join env (Just j) = second Just <$> zonkSyntaxExpr env j

    get_pat (_, ApplicativeArgOne pat _)    = pat
    get_pat (_, ApplicativeArgMany _ _ pat) = pat

    replace_pat pat (op, ApplicativeArgOne _ a)
      = (op, ApplicativeArgOne pat a)
    replace_pat pat (op, ApplicativeArgMany a b _)
      = (op, ApplicativeArgMany a b pat)

    zonk_args env args
      = do { (env1, new_args_rev) <- zonk_args_rev env (reverse args)
           ; (env2, new_pats)     <- zonkPats env1 (map get_pat args)
           ; return (env2, zipWith replace_pat new_pats (reverse new_args_rev)) }

     -- these need to go backward, because if any operators are higher-rank,
     -- later operators may introduce skolems that are in scope for earlier
     -- arguments
    zonk_args_rev env ((op, arg) : args)
      = do { (env1, new_op)         <- zonkSyntaxExpr env op
           ; new_arg                <- zonk_arg env1 arg
           ; (env2, new_args)       <- zonk_args_rev env1 args
           ; return (env2, (new_op, new_arg) : new_args) }
    zonk_args_rev env [] = return (env, [])

    zonk_arg env (ApplicativeArgOne pat expr)
      = do { new_expr <- zonkLExpr env expr
           ; return (ApplicativeArgOne pat new_expr) }
    zonk_arg env (ApplicativeArgMany stmts ret pat)
      = do { (env1, new_stmts) <- zonkStmts env zonkLExpr stmts
           ; new_ret           <- zonkExpr env1 ret
           ; return (ApplicativeArgMany new_stmts new_ret pat) }

-------------------------------------------------------------------------
zonkRecFields :: ZonkEnv -> HsRecordBinds GhcTcId -> TcM (HsRecordBinds GhcTcId)
zonkRecFields env (HsRecFields flds dd)
  = do  { flds' <- mapM zonk_rbind flds
        ; return (HsRecFields flds' dd) }
  where
    zonk_rbind (L l fld)
      = do { new_id   <- wrapLocM (zonkFieldOcc env) (hsRecFieldLbl fld)
           ; new_expr <- zonkLExpr env (hsRecFieldArg fld)
           ; return (L l (fld { hsRecFieldLbl = new_id
                              , hsRecFieldArg = new_expr })) }

zonkRecUpdFields :: ZonkEnv -> [LHsRecUpdField GhcTcId]
                 -> TcM [LHsRecUpdField GhcTcId]
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

zonkPat :: ZonkEnv -> OutPat GhcTcId -> TcM (ZonkEnv, OutPat GhcTc)
-- Extend the environment as we go, because it's possible for one
-- pattern to bind something that is used in another (inside or
-- to the right)
zonkPat env pat = wrapLocSndM (zonk_pat env) pat

zonk_pat :: ZonkEnv -> Pat GhcTcId -> TcM (ZonkEnv, Pat GhcTc)
zonk_pat env (ParPat p)
  = do  { (env', p') <- zonkPat env p
        ; return (env', ParPat p') }

zonk_pat env (WildPat ty)
  = do  { ty' <- zonkTcTypeToType env ty
        ; ensureNotLevPoly ty'
            (text "In a wildcard pattern")
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
  = do  { (env', wit') <- zonkSyntaxExpr env wit
        ; ty2' <- zonkTcTypeToType env' ty2
        ; ty' <- zonkTcTypeToType env' ty
        ; (env'', pats') <- zonkPats env' pats
        ; return (env'', ListPat pats' ty' (Just (ty2',wit'))) }

zonk_pat env (PArrPat pats ty)
  = do  { ty' <- zonkTcTypeToType env ty
        ; (env', pats') <- zonkPats env pats
        ; return (env', PArrPat pats' ty') }

zonk_pat env (TuplePat pats boxed tys)
  = do  { tys' <- mapM (zonkTcTypeToType env) tys
        ; (env', pats') <- zonkPats env pats
        ; return (env', TuplePat pats' boxed tys') }

zonk_pat env (SumPat pat alt arity tys)
  = do  { tys' <- mapM (zonkTcTypeToType env) tys
        ; (env', pat') <- zonkPat env pat
        ; return (env', SumPat pat' alt arity tys') }

zonk_pat env p@(ConPatOut { pat_arg_tys = tys, pat_tvs = tyvars
                          , pat_dicts = evs, pat_binds = binds
                          , pat_args = args, pat_wrap = wrapper
                          , pat_con = L _ con })
  = ASSERT( all isImmutableTyVar tyvars )
    do  { new_tys <- mapM (zonkTcTypeToType env) tys

          -- an unboxed tuple pattern (but only an unboxed tuple pattern)
          -- might have levity-polymorphic arguments. Check for this badness.
        ; case con of
            RealDataCon dc
              | isUnboxedTupleTyCon (dataConTyCon dc)
              -> mapM_ (checkForLevPoly doc) (dropRuntimeRepArgs new_tys)
            _ -> return ()

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
  where
    doc = text "In the type of an element of an unboxed tuple pattern:" $$ ppr p

zonk_pat env (LitPat lit) = return (env, LitPat lit)

zonk_pat env (SigPatOut pat ty)
  = do  { ty' <- zonkTcTypeToType env ty
        ; (env', pat') <- zonkPat env pat
        ; return (env', SigPatOut pat' ty') }

zonk_pat env (NPat (L l lit) mb_neg eq_expr ty)
  = do  { (env1, eq_expr') <- zonkSyntaxExpr env eq_expr
        ; (env2, mb_neg') <- case mb_neg of
            Nothing -> return (env1, Nothing)
            Just n  -> second Just <$> zonkSyntaxExpr env1 n

        ; lit' <- zonkOverLit env2 lit
        ; ty' <- zonkTcTypeToType env2 ty
        ; return (env2, NPat (L l lit') mb_neg' eq_expr' ty') }

zonk_pat env (NPlusKPat (L loc n) (L l lit1) lit2 e1 e2 ty)
  = do  { (env1, e1') <- zonkSyntaxExpr env  e1
        ; (env2, e2') <- zonkSyntaxExpr env1 e2
        ; n' <- zonkIdBndr env2 n
        ; lit1' <- zonkOverLit env2 lit1
        ; lit2' <- zonkOverLit env2 lit2
        ; ty' <- zonkTcTypeToType env2 ty
        ; return (extendIdZonkEnv1 env2 n',
                  NPlusKPat (L loc n') (L l lit1') lit2' e1' e2' ty') }

zonk_pat env (CoPat co_fn pat ty)
  = do { (env', co_fn') <- zonkCoFn env co_fn
       ; (env'', pat') <- zonkPat env' (noLoc pat)
       ; ty' <- zonkTcTypeToType env'' ty
       ; return (env'', CoPat co_fn' (unLoc pat') ty') }

zonk_pat _ pat = pprPanic "zonk_pat" (ppr pat)

---------------------------
zonkConStuff :: ZonkEnv
             -> HsConDetails (OutPat GhcTcId) (HsRecFields id (OutPat GhcTcId))
             -> TcM (ZonkEnv,
                    HsConDetails (OutPat GhcTc) (HsRecFields id (OutPat GhcTc)))
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
zonkPats :: ZonkEnv -> [OutPat GhcTcId] -> TcM (ZonkEnv, [OutPat GhcTc])
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

zonkForeignExports :: ZonkEnv -> [LForeignDecl GhcTcId]
                   -> TcM [LForeignDecl GhcTc]
zonkForeignExports env ls = mapM (wrapLocM (zonkForeignExport env)) ls

zonkForeignExport :: ZonkEnv -> ForeignDecl GhcTcId -> TcM (ForeignDecl GhcTc)
zonkForeignExport env (ForeignExport { fd_name = i, fd_co = co, fd_fe = spec })
  = return (ForeignExport { fd_name = fmap (zonkIdOcc env) i
                          , fd_sig_ty = undefined, fd_co = co
                          , fd_fe = spec })
zonkForeignExport _ for_imp
  = return for_imp     -- Foreign imports don't need zonking

zonkRules :: ZonkEnv -> [LRuleDecl GhcTcId] -> TcM [LRuleDecl GhcTc]
zonkRules env rs = mapM (wrapLocM (zonkRule env)) rs

zonkRule :: ZonkEnv -> RuleDecl GhcTcId -> TcM (RuleDecl GhcTc)
zonkRule env (HsRule name act (vars{-::[RuleBndr TcId]-}) lhs fv_lhs rhs fv_rhs)
  = do { (env_inside, new_bndrs) <- mapAccumLM zonk_bndr env vars

       ; let env_lhs = setZonkType env_inside zonkTvSkolemising
              -- See Note [Zonking the LHS of a RULE]

       ; new_lhs <- zonkLExpr env_lhs    lhs
       ; new_rhs <- zonkLExpr env_inside rhs

       ; return (HsRule name act new_bndrs new_lhs fv_lhs new_rhs fv_rhs) }
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

zonkVects :: ZonkEnv -> [LVectDecl GhcTcId] -> TcM [LVectDecl GhcTc]
zonkVects env = mapM (wrapLocM (zonkVect env))

zonkVect :: ZonkEnv -> VectDecl GhcTcId -> TcM (VectDecl GhcTc)
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
zonkEvTerm env (EvSelector sel_id tys tms)
  = do { sel_id' <- zonkIdBndr env sel_id
       ; tys'    <- zonkTcTypeToTypes env tys
       ; tms' <- mapM (zonkEvTerm env) tms
       ; return (EvSelector sel_id' tys' tms') }

zonkEvTypeable :: ZonkEnv -> EvTypeable -> TcM EvTypeable
zonkEvTypeable env (EvTypeableTyCon tycon e)
  = do { e'  <- mapM (zonkEvTerm env) e
       ; return $ EvTypeableTyCon tycon e' }
zonkEvTypeable env (EvTypeableTyApp t1 t2)
  = do { t1' <- zonkEvTerm env t1
       ; t2' <- zonkEvTerm env t2
       ; return (EvTypeableTyApp t1' t2') }
zonkEvTypeable env (EvTypeableTrFun t1 t2)
  = do { t1' <- zonkEvTerm env t1
       ; t2' <- zonkEvTerm env t2
       ; return (EvTypeableTrFun t1' t2') }
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
zonkEvBindsVar env (EvBindsVar { ebv_binds = ref })
  = do { bs <- readMutVar ref
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

-- Confused by zonking? See Note [What is zonking?] in TcMType.
zonkTcTypeToType :: ZonkEnv -> TcType -> TcM Type
zonkTcTypeToType = mapType zonk_tycomapper

zonkTcTypeToTypes :: ZonkEnv -> [TcType] -> TcM [Type]
zonkTcTypeToTypes env tys = mapM (zonkTcTypeToType env) tys

zonkCoToCo :: ZonkEnv -> Coercion -> TcM Coercion
zonkCoToCo = mapCoercion zonk_tycomapper

zonkSigType :: TcType -> TcM Type
-- Zonk the type obtained from a user type signature
-- We want to turn any quantified (forall'd) variables into TyVars
-- but we may find some free TcTyVars, and we want to leave them
-- completely alone.  They may even have unification variables inside
-- e.g.  f (x::a) = ...(e :: a -> a)....
-- The type sig for 'e' mentions a free 'a' which will be a
-- unification SigTv variable.
zonkSigType = zonkTcTypeToType (mkEmptyZonkEnv zonk_unbound_tv)
  where
    zonk_unbound_tv :: UnboundTyVarZonker
    zonk_unbound_tv tv = return (mkTyVarTy tv)

zonkTvSkolemising :: UnboundTyVarZonker
-- This variant is used for the LHS of rules
-- See Note [Zonking the LHS of a RULE].
zonkTvSkolemising tv
  = do { let tv' = mkTyVar (tyVarName tv) (tyVarKind tv)
                  -- NB: the kind of tv is already zonked
             ty = mkTyVarTy tv'
                  -- Make a proper TyVar (remember we
                  -- are now done with type checking)
       ; writeMetaTyVar tv ty
       ; return ty }

zonkTypeZapping :: UnboundTyVarZonker
-- This variant is used for everything except the LHS of rules
-- It zaps unbound type variables to Any, except for RuntimeRep
-- vars which it zonks to LiftedRep
-- Works on both types and kinds
zonkTypeZapping tv
  = do { let ty | isRuntimeRepVar tv = liftedRepTy
                | otherwise          = anyTypeOfKind (tyVarKind tv)
       ; writeMetaTyVar tv ty
       ; return ty }

---------------------------------------
{- Note [Zonking the LHS of a RULE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also DsBinds Note [Free tyvars on rule LHS]

We need to gather the type variables mentioned on the LHS so we can
quantify over them.  Example:
  data T a = C

  foo :: T a -> Int
  foo C = 1

  {-# RULES "myrule"  foo C = 1 #-}

After type checking the LHS becomes (foo alpha (C alpha)) and we do
not want to zap the unbound meta-tyvar 'alpha' to Any, because that
limits the applicability of the rule.  Instead, we want to quantify
over it!

We do this in two stages.

* During zonking, we skolemise the TcTyVar 'alpha' to TyVar 'a'.  We
  do this by using zonkTvSkolemising as the UnboundTyVarZonker in the
  ZonkEnv.  (This is in fact the whole reason that the ZonkEnv has a
  UnboundTyVarZonker.)

* In DsBinds, we quantify over it.  See DsBinds
  Note [Free tyvars on rule LHS]

Quantifying here is awkward because (a) the data type is big and (b)
finding the free type vars of an expression is necessarily monadic
operation. (consider /\a -> f @ b, where b is side-effected to a)
-}
