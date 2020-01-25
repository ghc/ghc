{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1996-1998


TcHsSyn: Specialisations of the @HsSyn@ syntax for the typechecker

This module is an extension of @HsSyn@ syntax, for use in the type
checker.
-}

{-# LANGUAGE CPP, TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module TcHsSyn (
        -- * Extracting types from HsSyn
        hsLitType, hsPatType, hsLPatType,

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
        zonkTopBndrs,
        ZonkEnv, ZonkFlexi(..), emptyZonkEnv, mkEmptyZonkEnv, initZonkEnv,
        zonkTyVarBinders, zonkTyVarBindersX, zonkTyVarBinderX,
        zonkTyBndrs, zonkTyBndrsX,
        zonkTcTypeToType,  zonkTcTypeToTypeX,
        zonkTcTypesToTypes, zonkTcTypesToTypesX,
        zonkTyVarOcc,
        zonkCoToCo,
        zonkEvBinds, zonkTcEvBinds,
        zonkTcMethInfoToMethInfoX,
        lookupTyVarOcc
  ) where

#include "HsVersions.h"

import GhcPrelude

import GHC.Hs
import Id
import IdInfo
import Predicate
import TcRnMonad
import PrelNames
import BuildTyCl ( TcMethInfo, MethInfo )
import TcType
import TcMType
import TcEnv   ( tcLookupGlobalOnly )
import TcEvidence
import TyCoPpr ( pprTyVar )
import TysPrim
import TyCon
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
import CoreSyn

import {-# SOURCE #-} TcSplice (runTopSplice)

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

hsLPatType :: LPat GhcTc -> Type
hsLPatType (L _ p) = hsPatType p

hsPatType :: Pat GhcTc -> Type
hsPatType (ParPat _ pat)                = hsLPatType pat
hsPatType (WildPat ty)                  = ty
hsPatType (VarPat _ lvar)               = idType (unLoc lvar)
hsPatType (BangPat _ pat)               = hsLPatType pat
hsPatType (LazyPat _ pat)               = hsLPatType pat
hsPatType (LitPat _ lit)                = hsLitType lit
hsPatType (AsPat _ var _)               = idType (unLoc var)
hsPatType (ViewPat ty _ _)              = ty
hsPatType (ListPat (ListPatTc ty Nothing) _)      = mkListTy ty
hsPatType (ListPat (ListPatTc _ (Just (ty,_))) _) = ty
hsPatType (TuplePat tys _ bx)           = mkTupleTy1 bx tys
                  -- See Note [Don't flatten tuples from HsSyn] in MkCore
hsPatType (SumPat tys _ _ _ )           = mkSumTy tys
hsPatType (ConPat { pat_con = lcon
                  , pat_con_ext = ConPatTc
                    { pat_arg_tys = tys
                    }
                  })
                                        = conLikeResTy (unLoc lcon) tys
hsPatType (SigPat ty _ _)               = ty
hsPatType (NPat ty _ _ _)               = ty
hsPatType (NPlusKPat ty _ _ _ _ _)      = ty
hsPatType (XPat (CoPat _ _ ty))         = ty
hsPatType SplicePat{}                   = panic "hsPatType: SplicePat"

hsLitType :: HsLit (GhcPass p) -> TcType
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
hsLitType (XLit nec)         = noExtCon nec

-- Overloaded literals. Here mainly because it uses isIntTy etc

shortCutLit :: DynFlags -> OverLitVal -> TcType -> Maybe (HsExpr GhcTcId)
shortCutLit dflags (HsIntegral int@(IL src neg i)) ty
  | isIntTy ty  && inIntRange  dflags i = Just (HsLit noExtField (HsInt noExtField int))
  | isWordTy ty && inWordRange dflags i = Just (mkLit wordDataCon (HsWordPrim src i))
  | isIntegerTy ty = Just (HsLit noExtField (HsInteger src i ty))
  | otherwise = shortCutLit dflags (HsFractional (integralFractionalLit neg i)) ty
        -- The 'otherwise' case is important
        -- Consider (3 :: Float).  Syntactically it looks like an IntLit,
        -- so we'll call shortCutIntLit, but of course it's a float
        -- This can make a big difference for programs with a lot of
        -- literals, compiled without -O

shortCutLit _ (HsFractional f) ty
  | isFloatTy ty  = Just (mkLit floatDataCon  (HsFloatPrim noExtField f))
  | isDoubleTy ty = Just (mkLit doubleDataCon (HsDoublePrim noExtField f))
  | otherwise     = Nothing

shortCutLit _ (HsIsString src s) ty
  | isStringTy ty = Just (HsLit noExtField (HsString src s))
  | otherwise     = Nothing

mkLit :: DataCon -> HsLit GhcTc -> HsExpr GhcTc
mkLit con lit = HsApp noExtField (nlHsDataCon con) (nlHsLit lit)

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

-- | See Note [The ZonkEnv]
-- Confused by zonking? See Note [What is zonking?] in TcMType.
data ZonkEnv  -- See Note [The ZonkEnv]
  = ZonkEnv { ze_flexi  :: ZonkFlexi
            , ze_tv_env :: TyCoVarEnv TyCoVar
            , ze_id_env :: IdEnv      Id
            , ze_meta_tv_env :: TcRef (TyVarEnv Type) }

{- Note [The ZonkEnv]
~~~~~~~~~~~~~~~~~~~~~
* ze_flexi :: ZonkFlexi says what to do with a
  unification variable that is still un-unified.
  See Note [Un-unified unification variables]

* ze_tv_env :: TyCoVarEnv TyCoVar promotes sharing. At a binding site
  of a tyvar or covar, we zonk the kind right away and add a mapping
  to the env. This prevents re-zonking the kind at every
  occurrence. But this is *just* an optimisation.

* ze_id_env : IdEnv Id promotes sharing among Ids, by making all
  occurrences of the Id point to a single zonked copy, built at the
  binding site.

  Unlike ze_tv_env, it is knot-tied: see extendIdZonkEnvRec.
  In a mutually recursive group
     rec { f = ...g...; g = ...f... }
  we want the occurrence of g to point to the one zonked Id for g,
  and the same for f.

  Because it is knot-tied, we must be careful to consult it lazily.
  Specifically, zonkIdOcc is not monadic.

* ze_meta_tv_env: see Note [Sharing when zonking to Type]


Notes:
  * We must be careful never to put coercion variables (which are Ids,
    after all) in the knot-tied ze_id_env, because coercions can
    appear in types, and we sometimes inspect a zonked type in this
    module.  [Question: where, precisely?]

  * In zonkTyVarOcc we consult ze_tv_env in a monadic context,
    a second reason that ze_tv_env can't be monadic.

  * An obvious suggestion would be to have one VarEnv Var to
    replace both ze_id_env and ze_tv_env, but that doesn't work
    because of the knot-tying stuff mentioned above.

Note [Un-unified unification variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What should we do if we find a Flexi unification variable?
There are three possibilities:

* DefaultFlexi: this is the common case, in situations like
     length @alpha ([] @alpha)
  It really doesn't matter what type we choose for alpha.  But
  we must choose a type!  We can't leave mutable unification
  variables floating around: after typecheck is complete, every
  type variable occurrence must have a binding site.

  So we default it to 'Any' of the right kind.

  All this works for both type and kind variables (indeed
  the two are the same thing).

* SkolemiseFlexi: is a special case for the LHS of RULES.
  See Note [Zonking the LHS of a RULE]

* RuntimeUnkFlexi: is a special case for the GHCi debugger.
  It's a way to have a variable that is not a mutuable
  unification variable, but doesn't have a binding site
  either.
-}

data ZonkFlexi   -- See Note [Un-unified unification variables]
  = DefaultFlexi    -- Default unbound unification variables to Any
  | SkolemiseFlexi  -- Skolemise unbound unification variables
                    -- See Note [Zonking the LHS of a RULE]
  | RuntimeUnkFlexi -- Used in the GHCi debugger

instance Outputable ZonkEnv where
  ppr (ZonkEnv { ze_tv_env = tv_env
               , ze_id_env = id_env })
    = text "ZE" <+> braces (vcat
         [ text "ze_tv_env =" <+> ppr tv_env
         , text "ze_id_env =" <+> ppr id_env ])

-- The EvBinds have to already be zonked, but that's usually the case.
emptyZonkEnv :: TcM ZonkEnv
emptyZonkEnv = mkEmptyZonkEnv DefaultFlexi

mkEmptyZonkEnv :: ZonkFlexi -> TcM ZonkEnv
mkEmptyZonkEnv flexi
  = do { mtv_env_ref <- newTcRef emptyVarEnv
       ; return (ZonkEnv { ze_flexi = flexi
                         , ze_tv_env = emptyVarEnv
                         , ze_id_env = emptyVarEnv
                         , ze_meta_tv_env = mtv_env_ref }) }

initZonkEnv :: (ZonkEnv -> TcM b) -> TcM b
initZonkEnv thing_inside = do { ze <- mkEmptyZonkEnv DefaultFlexi
                              ; thing_inside ze }

-- | Extend the knot-tied environment.
extendIdZonkEnvRec :: ZonkEnv -> [Var] -> ZonkEnv
extendIdZonkEnvRec ze@(ZonkEnv { ze_id_env = id_env }) ids
    -- NB: Don't look at the var to decide which env't to put it in. That
    -- would end up knot-tying all the env'ts.
  = ze { ze_id_env = extendVarEnvList id_env [(id,id) | id <- ids] }
  -- Given coercion variables will actually end up here. That's OK though:
  -- coercion variables are never looked up in the knot-tied env't, so zonking
  -- them simply doesn't get optimised. No one gets hurt. An improvement (?)
  -- would be to do SCC analysis in zonkEvBinds and then only knot-tie the
  -- recursive groups. But perhaps the time it takes to do the analysis is
  -- more than the savings.

extendZonkEnv :: ZonkEnv -> [Var] -> ZonkEnv
extendZonkEnv ze@(ZonkEnv { ze_tv_env = tyco_env, ze_id_env = id_env }) vars
  = ze { ze_tv_env = extendVarEnvList tyco_env [(tv,tv) | tv <- tycovars]
       , ze_id_env = extendVarEnvList id_env   [(id,id) | id <- ids] }
  where
    (tycovars, ids) = partition isTyCoVar vars

extendIdZonkEnv :: ZonkEnv -> Var -> ZonkEnv
extendIdZonkEnv ze@(ZonkEnv { ze_id_env = id_env }) id
  = ze { ze_id_env = extendVarEnv id_env id id }

extendTyZonkEnv :: ZonkEnv -> TyVar -> ZonkEnv
extendTyZonkEnv ze@(ZonkEnv { ze_tv_env = ty_env }) tv
  = ze { ze_tv_env = extendVarEnv ty_env tv tv }

setZonkType :: ZonkEnv -> ZonkFlexi -> ZonkEnv
setZonkType ze flexi = ze { ze_flexi = flexi }

zonkEnvIds :: ZonkEnv -> TypeEnv
zonkEnvIds (ZonkEnv { ze_id_env = id_env})
  = mkNameEnv [(getName id, AnId id) | id <- nonDetEltsUFM id_env]
  -- It's OK to use nonDetEltsUFM here because we forget the ordering
  -- immediately by creating a TypeEnv

zonkLIdOcc :: ZonkEnv -> Located TcId -> Located Id
zonkLIdOcc env = mapLoc (zonkIdOcc env)

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
zonkIdOcc (ZonkEnv { ze_id_env = id_env}) id
  | isLocalVar id = lookupVarEnv id_env id `orElse`
                    id
  | otherwise     = id

zonkIdOccs :: ZonkEnv -> [TcId] -> [Id]
zonkIdOccs env ids = map (zonkIdOcc env) ids

-- zonkIdBndr is used *after* typechecking to get the Id's type
-- to its final form.  The TyVarEnv give
zonkIdBndr :: ZonkEnv -> TcId -> TcM Id
zonkIdBndr env v
  = do ty' <- zonkTcTypeToTypeX env (idType v)
       ensureNotLevPoly ty'
         (text "In the type of binder" <+> quotes (ppr v))

       return (modifyIdInfo (`setLevityInfoWithType` ty') (setIdType v ty'))

zonkIdBndrs :: ZonkEnv -> [TcId] -> TcM [Id]
zonkIdBndrs env ids = mapM (zonkIdBndr env) ids

zonkTopBndrs :: [TcId] -> TcM [Id]
zonkTopBndrs ids = initZonkEnv $ \ ze -> zonkIdBndrs ze ids

zonkFieldOcc :: ZonkEnv -> FieldOcc GhcTcId -> TcM (FieldOcc GhcTc)
zonkFieldOcc env (FieldOcc sel lbl)
  = fmap ((flip FieldOcc) lbl) $ zonkIdBndr env sel
zonkFieldOcc _ (XFieldOcc nec) = noExtCon nec

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
           zonkTcTypeToTypeX env var_ty
       ; return (setVarType var ty) }

{-
zonkEvVarOcc :: ZonkEnv -> EvVar -> TcM EvTerm
zonkEvVarOcc env v
  | isCoVar v
  = EvCoercion <$> zonkCoVarOcc env v
  | otherwise
  = return (EvId $ zonkIdOcc env v)
-}

zonkCoreBndrX :: ZonkEnv -> Var -> TcM (ZonkEnv, Var)
zonkCoreBndrX env v
  | isId v = do { v' <- zonkIdBndr env v
                ; return (extendIdZonkEnv env v', v') }
  | otherwise = zonkTyBndrX env v

zonkCoreBndrsX :: ZonkEnv -> [Var] -> TcM (ZonkEnv, [Var])
zonkCoreBndrsX = mapAccumLM zonkCoreBndrX

zonkTyBndrs :: [TcTyVar] -> TcM (ZonkEnv, [TyVar])
zonkTyBndrs tvs = initZonkEnv $ \ze -> zonkTyBndrsX ze tvs

zonkTyBndrsX :: ZonkEnv -> [TcTyVar] -> TcM (ZonkEnv, [TyVar])
zonkTyBndrsX = mapAccumLM zonkTyBndrX

zonkTyBndrX :: ZonkEnv -> TcTyVar -> TcM (ZonkEnv, TyVar)
-- This guarantees to return a TyVar (not a TcTyVar)
-- then we add it to the envt, so all occurrences are replaced
--
-- It does not clone: the new TyVar has the sane Name
-- as the old one.  This important when zonking the
-- TyVarBndrs of a TyCon, whose Names may scope.
zonkTyBndrX env tv
  = ASSERT2( isImmutableTyVar tv, ppr tv <+> dcolon <+> ppr (tyVarKind tv) )
    do { ki <- zonkTcTypeToTypeX env (tyVarKind tv)
               -- Internal names tidy up better, for iface files.
       ; let tv' = mkTyVar (tyVarName tv) ki
       ; return (extendTyZonkEnv env tv', tv') }

zonkTyVarBinders ::  [VarBndr TcTyVar vis]
                 -> TcM (ZonkEnv, [VarBndr TyVar vis])
zonkTyVarBinders tvbs = initZonkEnv $ \ ze -> zonkTyVarBindersX ze tvbs

zonkTyVarBindersX :: ZonkEnv -> [VarBndr TcTyVar vis]
                             -> TcM (ZonkEnv, [VarBndr TyVar vis])
zonkTyVarBindersX = mapAccumLM zonkTyVarBinderX

zonkTyVarBinderX :: ZonkEnv -> VarBndr TcTyVar vis
                            -> TcM (ZonkEnv, VarBndr TyVar vis)
-- Takes a TcTyVar and guarantees to return a TyVar
zonkTyVarBinderX env (Bndr tv vis)
  = do { (env', tv') <- zonkTyBndrX env tv
       ; return (env', Bndr tv' vis) }

zonkTopExpr :: HsExpr GhcTcId -> TcM (HsExpr GhcTc)
zonkTopExpr e = initZonkEnv $ \ ze -> zonkExpr ze e

zonkTopLExpr :: LHsExpr GhcTcId -> TcM (LHsExpr GhcTc)
zonkTopLExpr e = initZonkEnv $ \ ze -> zonkLExpr ze e

zonkTopDecls :: Bag EvBind
             -> LHsBinds GhcTcId
             -> [LRuleDecl GhcTcId] -> [LTcSpecPrag]
             -> [LForeignDecl GhcTcId]
             -> TcM (TypeEnv,
                     Bag EvBind,
                     LHsBinds GhcTc,
                     [LForeignDecl GhcTc],
                     [LTcSpecPrag],
                     [LRuleDecl    GhcTc])
zonkTopDecls ev_binds binds rules imp_specs fords
  = do  { (env1, ev_binds') <- initZonkEnv $ \ ze -> zonkEvBinds ze ev_binds
        ; (env2, binds')    <- zonkRecMonoBinds env1 binds
                        -- Top level is implicitly recursive
        ; rules' <- zonkRules env2 rules
        ; specs' <- zonkLTcSpecPrags env2 imp_specs
        ; fords' <- zonkForeignExports env2 fords
        ; return (zonkEnvIds env2, ev_binds', binds', fords', specs', rules') }

---------------------------------------------
zonkLocalBinds :: ZonkEnv -> HsLocalBinds GhcTcId
               -> TcM (ZonkEnv, HsLocalBinds GhcTc)
zonkLocalBinds env (EmptyLocalBinds x)
  = return (env, (EmptyLocalBinds x))

zonkLocalBinds _ (HsValBinds _ (ValBinds {}))
  = panic "zonkLocalBinds" -- Not in typechecker output

zonkLocalBinds env (HsValBinds x (XValBindsLR (NValBinds binds sigs)))
  = do  { (env1, new_binds) <- go env binds
        ; return (env1, HsValBinds x (XValBindsLR (NValBinds new_binds sigs))) }
  where
    go env []
      = return (env, [])
    go env ((r,b):bs)
      = do { (env1, b')  <- zonkRecMonoBinds env b
           ; (env2, bs') <- go env1 bs
           ; return (env2, (r,b'):bs') }

zonkLocalBinds env (HsIPBinds x (IPBinds dict_binds binds )) = do
    new_binds <- mapM (wrapLocM zonk_ip_bind) binds
    let
        env1 = extendIdZonkEnvRec env
                 [ n | (L _ (IPBind _ (Right n) _)) <- new_binds]
    (env2, new_dict_binds) <- zonkTcEvBinds env1 dict_binds
    return (env2, HsIPBinds x (IPBinds new_dict_binds new_binds))
  where
    zonk_ip_bind (IPBind x n e)
        = do n' <- mapIPNameTc (zonkIdBndr env) n
             e' <- zonkLExpr env e
             return (IPBind x n' e')
    zonk_ip_bind (XIPBind nec) = noExtCon nec

zonkLocalBinds _ (HsIPBinds _ (XHsIPBinds nec))
  = noExtCon nec
zonkLocalBinds _ (XHsLocalBindsLR nec)
  = noExtCon nec

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
zonk_bind env bind@(PatBind { pat_lhs = pat, pat_rhs = grhss
                            , pat_ext = NPatBindTc fvs ty})
  = do  { (_env, new_pat) <- zonkPat env pat            -- Env already extended
        ; new_grhss <- zonkGRHSs env zonkLExpr grhss
        ; new_ty    <- zonkTcTypeToTypeX env ty
        ; return (bind { pat_lhs = new_pat, pat_rhs = new_grhss
                       , pat_ext = NPatBindTc fvs new_ty }) }

zonk_bind env (VarBind { var_ext = x
                       , var_id = var, var_rhs = expr, var_inline = inl })
  = do { new_var  <- zonkIdBndr env var
       ; new_expr <- zonkLExpr env expr
       ; return (VarBind { var_ext = x
                         , var_id = new_var
                         , var_rhs = new_expr
                         , var_inline = inl }) }

zonk_bind env bind@(FunBind { fun_id = L loc var
                            , fun_matches = ms
                            , fun_ext = co_fn })
  = do { new_var <- zonkIdBndr env var
       ; (env1, new_co_fn) <- zonkCoFn env co_fn
       ; new_ms <- zonkMatchGroup env1 zonkLExpr ms
       ; return (bind { fun_id = L loc new_var
                      , fun_matches = new_ms
                      , fun_ext = new_co_fn }) }

zonk_bind env (AbsBinds { abs_tvs = tyvars, abs_ev_vars = evs
                        , abs_ev_binds = ev_binds
                        , abs_exports = exports
                        , abs_binds = val_binds
                        , abs_sig = has_sig })
  = ASSERT( all isImmutableTyVar tyvars )
    do { (env0, new_tyvars) <- zonkTyBndrsX env tyvars
       ; (env1, new_evs) <- zonkEvBndrsX env0 evs
       ; (env2, new_ev_binds) <- zonkTcEvBinds_s env1 ev_binds
       ; (new_val_bind, new_exports) <- fixM $ \ ~(new_val_binds, _) ->
         do { let env3 = extendIdZonkEnvRec env2 $
                         collectHsBindsBinders new_val_binds
            ; new_val_binds <- mapBagM (zonk_val_bind env3) val_binds
            ; new_exports   <- mapM (zonk_export env3) exports
            ; return (new_val_binds, new_exports) }
       ; return (AbsBinds { abs_ext = noExtField
                          , abs_tvs = new_tyvars, abs_ev_vars = new_evs
                          , abs_ev_binds = new_ev_binds
                          , abs_exports = new_exports, abs_binds = new_val_bind
                          , abs_sig = has_sig }) }
  where
    zonk_val_bind env lbind
      | has_sig
      , (L loc bind@(FunBind { fun_id      = L mloc mono_id
                             , fun_matches = ms
                             , fun_ext     = co_fn })) <- lbind
      = do { new_mono_id <- updateVarTypeM (zonkTcTypeToTypeX env) mono_id
                            -- Specifically /not/ zonkIdBndr; we do not
                            -- want to complain about a levity-polymorphic binder
           ; (env', new_co_fn) <- zonkCoFn env co_fn
           ; new_ms            <- zonkMatchGroup env' zonkLExpr ms
           ; return $ L loc $
             bind { fun_id      = L mloc new_mono_id
                  , fun_matches = new_ms
                  , fun_ext     = new_co_fn } }
      | otherwise
      = zonk_lbind env lbind   -- The normal case

    zonk_export env (ABE{ abe_ext = x
                        , abe_wrap = wrap
                        , abe_poly = poly_id
                        , abe_mono = mono_id
                        , abe_prags = prags })
        = do new_poly_id <- zonkIdBndr env poly_id
             (_, new_wrap) <- zonkCoFn env wrap
             new_prags <- zonkSpecPrags env prags
             return (ABE{ abe_ext = x
                        , abe_wrap = new_wrap
                        , abe_poly = new_poly_id
                        , abe_mono = zonkIdOcc env mono_id
                        , abe_prags = new_prags })
    zonk_export _ (XABExport nec) = noExtCon nec

zonk_bind env (PatSynBind x bind@(PSB { psb_id = L loc id
                                      , psb_args = details
                                      , psb_def = lpat
                                      , psb_dir = dir }))
  = do { id' <- zonkIdBndr env id
       ; (env1, lpat') <- zonkPat env lpat
       ; let details' = zonkPatSynDetails env1 details
       ; (_env2, dir') <- zonkPatSynDir env1 dir
       ; return $ PatSynBind x $
                  bind { psb_id = L loc id'
                       , psb_args = details'
                       , psb_def = lpat'
                       , psb_dir = dir' } }

zonk_bind _ (PatSynBind _ (XPatSynBind nec)) = noExtCon nec
zonk_bind _ (XHsBindsLR nec)                 = noExtCon nec

zonkPatSynDetails :: ZonkEnv
                  -> HsPatSynDetails (Located TcId)
                  -> HsPatSynDetails (Located Id)
zonkPatSynDetails env (PrefixCon as)
  = PrefixCon (map (zonkLIdOcc env) as)
zonkPatSynDetails env (InfixCon a1 a2)
  = InfixCon (zonkLIdOcc env a1) (zonkLIdOcc env a2)
zonkPatSynDetails env (RecCon flds)
  = RecCon (map (fmap (zonkLIdOcc env)) flds)

zonkPatSynDir :: ZonkEnv -> HsPatSynDir GhcTcId
              -> TcM (ZonkEnv, HsPatSynDir GhcTc)
zonkPatSynDir env Unidirectional        = return (env, Unidirectional)
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
zonkMatchGroup env zBody (MG { mg_alts = L l ms
                             , mg_ext = MatchGroupTc arg_tys res_ty
                             , mg_origin = origin })
  = do  { ms' <- mapM (zonkMatch env zBody) ms
        ; arg_tys' <- zonkTcTypesToTypesX env arg_tys
        ; res_ty'  <- zonkTcTypeToTypeX env res_ty
        ; return (MG { mg_alts = L l ms'
                     , mg_ext = MatchGroupTc arg_tys' res_ty'
                     , mg_origin = origin }) }
zonkMatchGroup _ _ (XMatchGroup nec) = noExtCon nec

zonkMatch :: ZonkEnv
          -> (ZonkEnv -> Located (body GhcTcId) -> TcM (Located (body GhcTc)))
          -> LMatch GhcTcId (Located (body GhcTcId))
          -> TcM (LMatch GhcTc (Located (body GhcTc)))
zonkMatch env zBody (L loc match@(Match { m_pats = pats
                                        , m_grhss = grhss }))
  = do  { (env1, new_pats) <- zonkPats env pats
        ; new_grhss <- zonkGRHSs env1 zBody grhss
        ; return (L loc (match { m_pats = new_pats, m_grhss = new_grhss })) }
zonkMatch _ _ (L  _ (XMatch nec)) = noExtCon nec

-------------------------------------------------------------------------
zonkGRHSs :: ZonkEnv
          -> (ZonkEnv -> Located (body GhcTcId) -> TcM (Located (body GhcTc)))
          -> GRHSs GhcTcId (Located (body GhcTcId))
          -> TcM (GRHSs GhcTc (Located (body GhcTc)))

zonkGRHSs env zBody (GRHSs x grhss (L l binds)) = do
    (new_env, new_binds) <- zonkLocalBinds env binds
    let
        zonk_grhs (GRHS xx guarded rhs)
          = do (env2, new_guarded) <- zonkStmts new_env zonkLExpr guarded
               new_rhs <- zBody env2 rhs
               return (GRHS xx new_guarded new_rhs)
        zonk_grhs (XGRHS nec) = noExtCon nec
    new_grhss <- mapM (wrapLocM zonk_grhs) grhss
    return (GRHSs x new_grhss (L l new_binds))
zonkGRHSs _ _ (XGRHSs nec) = noExtCon nec

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

zonkExpr env (HsVar x (L l id))
  = ASSERT2( isNothing (isDataConId_maybe id), ppr id )
    return (HsVar x (L l (zonkIdOcc env id)))

zonkExpr _ e@(HsConLikeOut {}) = return e

zonkExpr _ (HsIPVar x id)
  = return (HsIPVar x id)

zonkExpr _ e@HsOverLabel{} = return e

zonkExpr env (HsLit x (HsRat e f ty))
  = do new_ty <- zonkTcTypeToTypeX env ty
       return (HsLit x (HsRat e f new_ty))

zonkExpr _ (HsLit x lit)
  = return (HsLit x lit)

zonkExpr env (HsOverLit x lit)
  = do  { lit' <- zonkOverLit env lit
        ; return (HsOverLit x lit') }

zonkExpr env (HsLam x matches)
  = do new_matches <- zonkMatchGroup env zonkLExpr matches
       return (HsLam x new_matches)

zonkExpr env (HsLamCase x matches)
  = do new_matches <- zonkMatchGroup env zonkLExpr matches
       return (HsLamCase x new_matches)

zonkExpr env (HsApp x e1 e2)
  = do new_e1 <- zonkLExpr env e1
       new_e2 <- zonkLExpr env e2
       return (HsApp x new_e1 new_e2)

zonkExpr env (HsAppType x e t)
  = do new_e <- zonkLExpr env e
       return (HsAppType x new_e t)
       -- NB: the type is an HsType; can't zonk that!

zonkExpr _ e@(HsRnBracketOut _ _ _)
  = pprPanic "zonkExpr: HsRnBracketOut" (ppr e)

zonkExpr env (HsTcBracketOut x wrap body bs)
  = do wrap' <- traverse zonkQuoteWrap wrap
       bs' <- mapM (zonk_b env) bs
       return (HsTcBracketOut x wrap' body bs')
  where
    zonkQuoteWrap (QuoteWrapper ev ty) = do
        let ev' = zonkIdOcc env ev
        ty' <- zonkTcTypeToTypeX env ty
        return (QuoteWrapper ev' ty')

    zonk_b env' (PendingTcSplice n e) = do e' <- zonkLExpr env' e
                                           return (PendingTcSplice n e')

zonkExpr env (HsSpliceE _ (HsSplicedT s)) =
  runTopSplice s >>= zonkExpr env

zonkExpr _ (HsSpliceE x s) = WARN( True, ppr s ) -- Should not happen
                           return (HsSpliceE x s)

zonkExpr env (OpApp fixity e1 op e2)
  = do new_e1 <- zonkLExpr env e1
       new_op <- zonkLExpr env op
       new_e2 <- zonkLExpr env e2
       return (OpApp fixity new_e1 new_op new_e2)

zonkExpr env (NegApp x expr op)
  = do (env', new_op) <- zonkSyntaxExpr env op
       new_expr <- zonkLExpr env' expr
       return (NegApp x new_expr new_op)

zonkExpr env (HsPar x e)
  = do new_e <- zonkLExpr env e
       return (HsPar x new_e)

zonkExpr env (SectionL x expr op)
  = do new_expr <- zonkLExpr env expr
       new_op   <- zonkLExpr env op
       return (SectionL x new_expr new_op)

zonkExpr env (SectionR x op expr)
  = do new_op   <- zonkLExpr env op
       new_expr <- zonkLExpr env expr
       return (SectionR x new_op new_expr)

zonkExpr env (ExplicitTuple x tup_args boxed)
  = do { new_tup_args <- mapM zonk_tup_arg tup_args
       ; return (ExplicitTuple x new_tup_args boxed) }
  where
    zonk_tup_arg (L l (Present x e)) = do { e' <- zonkLExpr env e
                                          ; return (L l (Present x e')) }
    zonk_tup_arg (L l (Missing t)) = do { t' <- zonkTcTypeToTypeX env t
                                        ; return (L l (Missing t')) }
    zonk_tup_arg (L _ (XTupArg nec)) = noExtCon nec


zonkExpr env (ExplicitSum args alt arity expr)
  = do new_args <- mapM (zonkTcTypeToTypeX env) args
       new_expr <- zonkLExpr env expr
       return (ExplicitSum new_args alt arity new_expr)

zonkExpr env (HsCase x expr ms)
  = do new_expr <- zonkLExpr env expr
       new_ms <- zonkMatchGroup env zonkLExpr ms
       return (HsCase x new_expr new_ms)

zonkExpr env (HsIf x fun e1 e2 e3)
  = do (env1, new_fun) <- zonkSyntaxExpr env fun
       new_e1 <- zonkLExpr env1 e1
       new_e2 <- zonkLExpr env1 e2
       new_e3 <- zonkLExpr env1 e3
       return (HsIf x new_fun new_e1 new_e2 new_e3)

zonkExpr env (HsMultiIf ty alts)
  = do { alts' <- mapM (wrapLocM zonk_alt) alts
       ; ty'   <- zonkTcTypeToTypeX env ty
       ; return $ HsMultiIf ty' alts' }
  where zonk_alt (GRHS x guard expr)
          = do { (env', guard') <- zonkStmts env zonkLExpr guard
               ; expr'          <- zonkLExpr env' expr
               ; return $ GRHS x guard' expr' }
        zonk_alt (XGRHS nec) = noExtCon nec

zonkExpr env (HsLet x (L l binds) expr)
  = do (new_env, new_binds) <- zonkLocalBinds env binds
       new_expr <- zonkLExpr new_env expr
       return (HsLet x (L l new_binds) new_expr)

zonkExpr env (HsDo ty do_or_lc (L l stmts))
  = do (_, new_stmts) <- zonkStmts env zonkLExpr stmts
       new_ty <- zonkTcTypeToTypeX env ty
       return (HsDo new_ty do_or_lc (L l new_stmts))

zonkExpr env (ExplicitList ty wit exprs)
  = do (env1, new_wit) <- zonkWit env wit
       new_ty <- zonkTcTypeToTypeX env1 ty
       new_exprs <- zonkLExprs env1 exprs
       return (ExplicitList new_ty new_wit new_exprs)
   where zonkWit env Nothing    = return (env, Nothing)
         zonkWit env (Just fln) = second Just <$> zonkSyntaxExpr env fln

zonkExpr env expr@(RecordCon { rcon_ext = ext, rcon_flds = rbinds })
  = do  { new_con_expr <- zonkExpr env (rcon_con_expr ext)
        ; new_rbinds   <- zonkRecFields env rbinds
        ; return (expr { rcon_ext  = ext { rcon_con_expr = new_con_expr }
                       , rcon_flds = new_rbinds }) }

zonkExpr env (RecordUpd { rupd_flds = rbinds
                        , rupd_expr = expr
                        , rupd_ext = RecordUpdTc
                            { rupd_cons = cons, rupd_in_tys = in_tys
                            , rupd_out_tys = out_tys, rupd_wrap = req_wrap }})
  = do  { new_expr    <- zonkLExpr env expr
        ; new_in_tys  <- mapM (zonkTcTypeToTypeX env) in_tys
        ; new_out_tys <- mapM (zonkTcTypeToTypeX env) out_tys
        ; new_rbinds  <- zonkRecUpdFields env rbinds
        ; (_, new_recwrap) <- zonkCoFn env req_wrap
        ; return (RecordUpd { rupd_expr = new_expr, rupd_flds =  new_rbinds
                            , rupd_ext = RecordUpdTc
                                { rupd_cons = cons, rupd_in_tys = new_in_tys
                                , rupd_out_tys = new_out_tys
                                , rupd_wrap = new_recwrap }}) }

zonkExpr env (ExprWithTySig _ e ty)
  = do { e' <- zonkLExpr env e
       ; return (ExprWithTySig noExtField e' ty) }

zonkExpr env (ArithSeq expr wit info)
  = do (env1, new_wit) <- zonkWit env wit
       new_expr <- zonkExpr env expr
       new_info <- zonkArithSeq env1 info
       return (ArithSeq new_expr new_wit new_info)
   where zonkWit env Nothing    = return (env, Nothing)
         zonkWit env (Just fln) = second Just <$> zonkSyntaxExpr env fln

zonkExpr env (HsPragE x prag expr)
  = do new_expr <- zonkLExpr env expr
       return (HsPragE x prag new_expr)

-- arrow notation extensions
zonkExpr env (HsProc x pat body)
  = do  { (env1, new_pat) <- zonkPat env pat
        ; new_body <- zonkCmdTop env1 body
        ; return (HsProc x new_pat new_body) }

-- StaticPointers extension
zonkExpr env (HsStatic fvs expr)
  = HsStatic fvs <$> zonkLExpr env expr

zonkExpr env (XExpr (HsWrap co_fn expr))
  = do (env1, new_co_fn) <- zonkCoFn env co_fn
       new_expr <- zonkExpr env1 expr
       return (XExpr (HsWrap new_co_fn new_expr))

zonkExpr _ e@(HsUnboundVar {})
  = return e

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
zonkSyntaxExpr env (SyntaxExprTc { syn_expr      = expr
                               , syn_arg_wraps = arg_wraps
                               , syn_res_wrap  = res_wrap })
  = do { (env0, res_wrap')  <- zonkCoFn env res_wrap
       ; expr'              <- zonkExpr env0 expr
       ; (env1, arg_wraps') <- mapAccumLM zonkCoFn env0 arg_wraps
       ; return (env1, SyntaxExprTc { syn_expr      = expr'
                                    , syn_arg_wraps = arg_wraps'
                                    , syn_res_wrap  = res_wrap' }) }
zonkSyntaxExpr env NoSyntaxExprTc = return (env, NoSyntaxExprTc)

-------------------------------------------------------------------------

zonkLCmd  :: ZonkEnv -> LHsCmd GhcTcId   -> TcM (LHsCmd GhcTc)
zonkCmd   :: ZonkEnv -> HsCmd GhcTcId    -> TcM (HsCmd GhcTc)

zonkLCmd  env cmd  = wrapLocM (zonkCmd env) cmd

zonkCmd env (XCmd (HsWrap w cmd))
  = do { (env1, w') <- zonkCoFn env w
       ; cmd' <- zonkCmd env1 cmd
       ; return (XCmd (HsWrap w' cmd')) }
zonkCmd env (HsCmdArrApp ty e1 e2 ho rl)
  = do new_e1 <- zonkLExpr env e1
       new_e2 <- zonkLExpr env e2
       new_ty <- zonkTcTypeToTypeX env ty
       return (HsCmdArrApp new_ty new_e1 new_e2 ho rl)

zonkCmd env (HsCmdArrForm x op f fixity args)
  = do new_op <- zonkLExpr env op
       new_args <- mapM (zonkCmdTop env) args
       return (HsCmdArrForm x new_op f fixity new_args)

zonkCmd env (HsCmdApp x c e)
  = do new_c <- zonkLCmd env c
       new_e <- zonkLExpr env e
       return (HsCmdApp x new_c new_e)

zonkCmd env (HsCmdLam x matches)
  = do new_matches <- zonkMatchGroup env zonkLCmd matches
       return (HsCmdLam x new_matches)

zonkCmd env (HsCmdPar x c)
  = do new_c <- zonkLCmd env c
       return (HsCmdPar x new_c)

zonkCmd env (HsCmdCase x expr ms)
  = do new_expr <- zonkLExpr env expr
       new_ms <- zonkMatchGroup env zonkLCmd ms
       return (HsCmdCase x new_expr new_ms)

zonkCmd env (HsCmdIf x eCond ePred cThen cElse)
  = do { (env1, new_eCond) <- zonkSyntaxExpr env eCond
       ; new_ePred <- zonkLExpr env1 ePred
       ; new_cThen <- zonkLCmd env1 cThen
       ; new_cElse <- zonkLCmd env1 cElse
       ; return (HsCmdIf x new_eCond new_ePred new_cThen new_cElse) }

zonkCmd env (HsCmdLet x (L l binds) cmd)
  = do (new_env, new_binds) <- zonkLocalBinds env binds
       new_cmd <- zonkLCmd new_env cmd
       return (HsCmdLet x (L l new_binds) new_cmd)

zonkCmd env (HsCmdDo ty (L l stmts))
  = do (_, new_stmts) <- zonkStmts env zonkLCmd stmts
       new_ty <- zonkTcTypeToTypeX env ty
       return (HsCmdDo new_ty (L l new_stmts))



zonkCmdTop :: ZonkEnv -> LHsCmdTop GhcTcId -> TcM (LHsCmdTop GhcTc)
zonkCmdTop env cmd = wrapLocM (zonk_cmd_top env) cmd

zonk_cmd_top :: ZonkEnv -> HsCmdTop GhcTcId -> TcM (HsCmdTop GhcTc)
zonk_cmd_top env (HsCmdTop (CmdTopTc stack_tys ty ids) cmd)
  = do new_cmd <- zonkLCmd env cmd
       new_stack_tys <- zonkTcTypeToTypeX env stack_tys
       new_ty <- zonkTcTypeToTypeX env ty
       new_ids <- mapSndM (zonkExpr env) ids

       MASSERT( isLiftedTypeKind (tcTypeKind new_stack_tys) )
         -- desugarer assumes that this is not levity polymorphic...
         -- but indeed it should always be lifted due to the typing
         -- rules for arrows

       return (HsCmdTop (CmdTopTc new_stack_tys new_ty new_ids) new_cmd)
zonk_cmd_top _ (XCmdTop nec) = noExtCon nec

-------------------------------------------------------------------------
zonkCoFn :: ZonkEnv -> HsWrapper -> TcM (ZonkEnv, HsWrapper)
zonkCoFn env WpHole   = return (env, WpHole)
zonkCoFn env (WpCompose c1 c2) = do { (env1, c1') <- zonkCoFn env c1
                                    ; (env2, c2') <- zonkCoFn env1 c2
                                    ; return (env2, WpCompose c1' c2') }
zonkCoFn env (WpFun c1 c2 t1 d) = do { (env1, c1') <- zonkCoFn env c1
                                     ; (env2, c2') <- zonkCoFn env1 c2
                                     ; t1'         <- zonkTcTypeToTypeX env2 t1
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
zonkCoFn env (WpTyApp ty)   = do { ty' <- zonkTcTypeToTypeX env ty
                                 ; return (env, WpTyApp ty') }
zonkCoFn env (WpLet bs)     = do { (env1, bs') <- zonkTcEvBinds env bs
                                 ; return (env1, WpLet bs') }

-------------------------------------------------------------------------
zonkOverLit :: ZonkEnv -> HsOverLit GhcTcId -> TcM (HsOverLit GhcTc)
zonkOverLit env lit@(OverLit {ol_ext = OverLitTc r ty, ol_witness = e })
  = do  { ty' <- zonkTcTypeToTypeX env ty
        ; e' <- zonkExpr env e
        ; return (lit { ol_witness = e', ol_ext = OverLitTc r ty' }) }

zonkOverLit _ (XOverLit nec) = noExtCon nec

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
zonkStmt env _ (ParStmt bind_ty stmts_w_bndrs mzip_op bind_op)
  = do { (env1, new_bind_op) <- zonkSyntaxExpr env bind_op
       ; new_bind_ty <- zonkTcTypeToTypeX env1 bind_ty
       ; new_stmts_w_bndrs <- mapM (zonk_branch env1) stmts_w_bndrs
       ; let new_binders = [b | ParStmtBlock _ _ bs _ <- new_stmts_w_bndrs
                              , b <- bs]
             env2 = extendIdZonkEnvRec env1 new_binders
       ; new_mzip <- zonkExpr env2 mzip_op
       ; return (env2
                , ParStmt new_bind_ty new_stmts_w_bndrs new_mzip new_bind_op)}
  where
    zonk_branch env1 (ParStmtBlock x stmts bndrs return_op)
       = do { (env2, new_stmts)  <- zonkStmts env1 zonkLExpr stmts
            ; (env3, new_return) <- zonkSyntaxExpr env2 return_op
            ; return (ParStmtBlock x new_stmts (zonkIdOccs env3 bndrs)
                                                                   new_return) }
    zonk_branch _ (XParStmtBlock nec) = noExtCon nec

zonkStmt env zBody (RecStmt { recS_stmts = segStmts, recS_later_ids = lvs, recS_rec_ids = rvs
                            , recS_ret_fn = ret_id, recS_mfix_fn = mfix_id
                            , recS_bind_fn = bind_id
                            , recS_ext =
                                       RecStmtTc { recS_bind_ty = bind_ty
                                                 , recS_later_rets = later_rets
                                                 , recS_rec_rets = rec_rets
                                                 , recS_ret_ty = ret_ty} })
  = do { (env1, new_bind_id) <- zonkSyntaxExpr env bind_id
       ; (env2, new_mfix_id) <- zonkSyntaxExpr env1 mfix_id
       ; (env3, new_ret_id)  <- zonkSyntaxExpr env2 ret_id
       ; new_bind_ty <- zonkTcTypeToTypeX env3 bind_ty
       ; new_rvs <- zonkIdBndrs env3 rvs
       ; new_lvs <- zonkIdBndrs env3 lvs
       ; new_ret_ty  <- zonkTcTypeToTypeX env3 ret_ty
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
                         , recS_ext = RecStmtTc
                             { recS_bind_ty = new_bind_ty
                             , recS_later_rets = new_later_rets
                             , recS_rec_rets = new_rec_rets
                             , recS_ret_ty = new_ret_ty } }) }

zonkStmt env zBody (BodyStmt ty body then_op guard_op)
  = do (env1, new_then_op)  <- zonkSyntaxExpr env then_op
       (env2, new_guard_op) <- zonkSyntaxExpr env1 guard_op
       new_body <- zBody env2 body
       new_ty   <- zonkTcTypeToTypeX env2 ty
       return (env2, BodyStmt new_ty new_body new_then_op new_guard_op)

zonkStmt env zBody (LastStmt x body noret ret_op)
  = do (env1, new_ret) <- zonkSyntaxExpr env ret_op
       new_body <- zBody env1 body
       return (env, LastStmt x new_body noret new_ret)

zonkStmt env _ (TransStmt { trS_stmts = stmts, trS_bndrs = binderMap
                          , trS_by = by, trS_form = form, trS_using = using
                          , trS_ret = return_op, trS_bind = bind_op
                          , trS_ext = bind_arg_ty
                          , trS_fmap = liftM_op })
  = do {
    ; (env1, bind_op') <- zonkSyntaxExpr env bind_op
    ; bind_arg_ty' <- zonkTcTypeToTypeX env1 bind_arg_ty
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
                               , trS_ext = bind_arg_ty'
                               , trS_fmap = liftM_op' }) }
  where
    zonkBinderMapEntry env  (oldBinder, newBinder) = do
        let oldBinder' = zonkIdOcc env oldBinder
        newBinder' <- zonkIdBndr env newBinder
        return (oldBinder', newBinder')

zonkStmt env _ (LetStmt x (L l binds))
  = do (env1, new_binds) <- zonkLocalBinds env binds
       return (env1, LetStmt x (L l new_binds))

zonkStmt env zBody (BindStmt bind_ty pat body bind_op fail_op)
  = do  { (env1, new_bind) <- zonkSyntaxExpr env bind_op
        ; new_bind_ty <- zonkTcTypeToTypeX env1 bind_ty
        ; new_body <- zBody env1 body
        ; (env2, new_pat) <- zonkPat env1 pat
        ; (_, new_fail) <- zonkSyntaxExpr env1 fail_op
        ; return ( env2
                 , BindStmt new_bind_ty new_pat new_body new_bind new_fail) }

-- Scopes: join > ops (in reverse order) > pats (in forward order)
--              > rest of stmts
zonkStmt env _zBody (ApplicativeStmt body_ty args mb_join)
  = do  { (env1, new_mb_join)   <- zonk_join env mb_join
        ; (env2, new_args)      <- zonk_args env1 args
        ; new_body_ty           <- zonkTcTypeToTypeX env2 body_ty
        ; return ( env2
                 , ApplicativeStmt new_body_ty new_args new_mb_join) }
  where
    zonk_join env Nothing  = return (env, Nothing)
    zonk_join env (Just j) = second Just <$> zonkSyntaxExpr env j

    get_pat (_, ApplicativeArgOne _ pat _ _ _) = pat
    get_pat (_, ApplicativeArgMany _ _ _ pat) = pat
    get_pat (_, XApplicativeArg nec) = noExtCon nec

    replace_pat pat (op, ApplicativeArgOne x _ a isBody fail_op)
      = (op, ApplicativeArgOne x pat a isBody fail_op)
    replace_pat pat (op, ApplicativeArgMany x a b _)
      = (op, ApplicativeArgMany x a b pat)
    replace_pat _ (_, XApplicativeArg nec) = noExtCon nec

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

    zonk_arg env (ApplicativeArgOne x pat expr isBody fail_op)
      = do { new_expr <- zonkLExpr env expr
           ; (_, new_fail) <- zonkSyntaxExpr env fail_op
           ; return (ApplicativeArgOne x pat new_expr isBody new_fail) }
    zonk_arg env (ApplicativeArgMany x stmts ret pat)
      = do { (env1, new_stmts) <- zonkStmts env zonkLExpr stmts
           ; new_ret           <- zonkExpr env1 ret
           ; return (ApplicativeArgMany x new_stmts new_ret pat) }
    zonk_arg _ (XApplicativeArg nec) = noExtCon nec

zonkStmt _ _ (XStmtLR nec) = noExtCon nec

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
zonk_pat env (ParPat x p)
  = do  { (env', p') <- zonkPat env p
        ; return (env', ParPat x p') }

zonk_pat env (WildPat ty)
  = do  { ty' <- zonkTcTypeToTypeX env ty
        ; ensureNotLevPoly ty'
            (text "In a wildcard pattern")
        ; return (env, WildPat ty') }

zonk_pat env (VarPat x (L l v))
  = do  { v' <- zonkIdBndr env v
        ; return (extendIdZonkEnv env v', VarPat x (L l v')) }

zonk_pat env (LazyPat x pat)
  = do  { (env', pat') <- zonkPat env pat
        ; return (env',  LazyPat x pat') }

zonk_pat env (BangPat x pat)
  = do  { (env', pat') <- zonkPat env pat
        ; return (env',  BangPat x pat') }

zonk_pat env (AsPat x (L loc v) pat)
  = do  { v' <- zonkIdBndr env v
        ; (env', pat') <- zonkPat (extendIdZonkEnv env v') pat
        ; return (env', AsPat x (L loc v') pat') }

zonk_pat env (ViewPat ty expr pat)
  = do  { expr' <- zonkLExpr env expr
        ; (env', pat') <- zonkPat env pat
        ; ty' <- zonkTcTypeToTypeX env ty
        ; return (env', ViewPat ty' expr' pat') }

zonk_pat env (ListPat (ListPatTc ty Nothing) pats)
  = do  { ty' <- zonkTcTypeToTypeX env ty
        ; (env', pats') <- zonkPats env pats
        ; return (env', ListPat (ListPatTc ty' Nothing) pats') }

zonk_pat env (ListPat (ListPatTc ty (Just (ty2,wit))) pats)
  = do  { (env', wit') <- zonkSyntaxExpr env wit
        ; ty2' <- zonkTcTypeToTypeX env' ty2
        ; ty' <- zonkTcTypeToTypeX env' ty
        ; (env'', pats') <- zonkPats env' pats
        ; return (env'', ListPat (ListPatTc ty' (Just (ty2',wit'))) pats') }

zonk_pat env (TuplePat tys pats boxed)
  = do  { tys' <- mapM (zonkTcTypeToTypeX env) tys
        ; (env', pats') <- zonkPats env pats
        ; return (env', TuplePat tys' pats' boxed) }

zonk_pat env (SumPat tys pat alt arity )
  = do  { tys' <- mapM (zonkTcTypeToTypeX env) tys
        ; (env', pat') <- zonkPat env pat
        ; return (env', SumPat tys' pat' alt arity) }

zonk_pat env p@(ConPat { pat_con = L _ con
                       , pat_args = args
                       , pat_con_ext = p'@(ConPatTc
                         { pat_tvs = tyvars
                         , pat_dicts = evs
                         , pat_binds = binds
                         , pat_wrap = wrapper
                         , pat_arg_tys = tys
                         })
                       })
  = ASSERT( all isImmutableTyVar tyvars )
    do  { new_tys <- mapM (zonkTcTypeToTypeX env) tys

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
        ; pure ( env'
               , p
                 { pat_args = new_args
                 , pat_con_ext = p'
                   { pat_arg_tys = new_tys
                   , pat_tvs = new_tyvars
                   , pat_dicts = new_evs
                   , pat_binds = new_binds
                   , pat_wrap = new_wrapper
                   }
                 }
               )
        }
  where
    doc = text "In the type of an element of an unboxed tuple pattern:" $$ ppr p

zonk_pat env (LitPat x lit) = return (env, LitPat x lit)

zonk_pat env (SigPat ty pat hs_ty)
  = do  { ty' <- zonkTcTypeToTypeX env ty
        ; (env', pat') <- zonkPat env pat
        ; return (env', SigPat ty' pat' hs_ty) }

zonk_pat env (NPat ty (L l lit) mb_neg eq_expr)
  = do  { (env1, eq_expr') <- zonkSyntaxExpr env eq_expr
        ; (env2, mb_neg') <- case mb_neg of
            Nothing -> return (env1, Nothing)
            Just n  -> second Just <$> zonkSyntaxExpr env1 n

        ; lit' <- zonkOverLit env2 lit
        ; ty' <- zonkTcTypeToTypeX env2 ty
        ; return (env2, NPat ty' (L l lit') mb_neg' eq_expr') }

zonk_pat env (NPlusKPat ty (L loc n) (L l lit1) lit2 e1 e2)
  = do  { (env1, e1') <- zonkSyntaxExpr env  e1
        ; (env2, e2') <- zonkSyntaxExpr env1 e2
        ; n' <- zonkIdBndr env2 n
        ; lit1' <- zonkOverLit env2 lit1
        ; lit2' <- zonkOverLit env2 lit2
        ; ty' <- zonkTcTypeToTypeX env2 ty
        ; return (extendIdZonkEnv env2 n',
                  NPlusKPat ty' (L loc n') (L l lit1') lit2' e1' e2') }

zonk_pat env (XPat (CoPat co_fn pat ty))
  = do { (env', co_fn') <- zonkCoFn env co_fn
       ; (env'', pat') <- zonkPat env' (noLoc pat)
       ; ty' <- zonkTcTypeToTypeX env'' ty
       ; return (env'', XPat $ CoPat co_fn' (unLoc pat') ty')
       }

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
        ; let rpats' = zipWith (\(L l rp) p' ->
                                  L l (rp { hsRecFieldArg = p' }))
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
zonkForeignExport env (ForeignExport { fd_name = i, fd_e_ext = co
                                     , fd_fe = spec })
  = return (ForeignExport { fd_name = zonkLIdOcc env i
                          , fd_sig_ty = undefined, fd_e_ext = co
                          , fd_fe = spec })
zonkForeignExport _ for_imp
  = return for_imp     -- Foreign imports don't need zonking

zonkRules :: ZonkEnv -> [LRuleDecl GhcTcId] -> TcM [LRuleDecl GhcTc]
zonkRules env rs = mapM (wrapLocM (zonkRule env)) rs

zonkRule :: ZonkEnv -> RuleDecl GhcTcId -> TcM (RuleDecl GhcTc)
zonkRule env rule@(HsRule { rd_tmvs = tm_bndrs{-::[RuleBndr TcId]-}
                          , rd_lhs = lhs
                          , rd_rhs = rhs })
  = do { (env_inside, new_tm_bndrs) <- mapAccumLM zonk_tm_bndr env tm_bndrs

       ; let env_lhs = setZonkType env_inside SkolemiseFlexi
              -- See Note [Zonking the LHS of a RULE]

       ; new_lhs <- zonkLExpr env_lhs    lhs
       ; new_rhs <- zonkLExpr env_inside rhs

       ; return $ rule { rd_tmvs = new_tm_bndrs
                       , rd_lhs  = new_lhs
                       , rd_rhs  = new_rhs } }
  where
   zonk_tm_bndr env (L l (RuleBndr x (L loc v)))
      = do { (env', v') <- zonk_it env v
           ; return (env', L l (RuleBndr x (L loc v'))) }
   zonk_tm_bndr _ (L _ (RuleBndrSig {})) = panic "zonk_tm_bndr RuleBndrSig"
   zonk_tm_bndr _ (L _ (XRuleBndr nec)) = noExtCon nec

   zonk_it env v
     | isId v     = do { v' <- zonkIdBndr env v
                       ; return (extendIdZonkEnvRec env [v'], v') }
     | otherwise  = ASSERT( isImmutableTyVar v)
                    zonkTyBndrX env v
                    -- DV: used to be return (env,v) but that is plain
                    -- wrong because we may need to go inside the kind
                    -- of v and zonk there!
zonkRule _ (XRuleDecl nec) = noExtCon nec

{-
************************************************************************
*                                                                      *
              Constraints and evidence
*                                                                      *
************************************************************************
-}

zonkEvTerm :: ZonkEnv -> EvTerm -> TcM EvTerm
zonkEvTerm env (EvExpr e)
  = EvExpr <$> zonkCoreExpr env e
zonkEvTerm env (EvTypeable ty ev)
  = EvTypeable <$> zonkTcTypeToTypeX env ty <*> zonkEvTypeable env ev
zonkEvTerm env (EvFun { et_tvs = tvs, et_given = evs
                      , et_binds = ev_binds, et_body = body_id })
  = do { (env0, new_tvs) <- zonkTyBndrsX env tvs
       ; (env1, new_evs) <- zonkEvBndrsX env0 evs
       ; (env2, new_ev_binds) <- zonkTcEvBinds env1 ev_binds
       ; let new_body_id = zonkIdOcc env2 body_id
       ; return (EvFun { et_tvs = new_tvs, et_given = new_evs
                       , et_binds = new_ev_binds, et_body = new_body_id }) }

zonkCoreExpr :: ZonkEnv -> CoreExpr -> TcM CoreExpr
zonkCoreExpr env (Var v)
    | isCoVar v
    = Coercion <$> zonkCoVarOcc env v
    | otherwise
    = return (Var $ zonkIdOcc env v)
zonkCoreExpr _ (Lit l)
    = return $ Lit l
zonkCoreExpr env (Coercion co)
    = Coercion <$> zonkCoToCo env co
zonkCoreExpr env (Type ty)
    = Type <$> zonkTcTypeToTypeX env ty

zonkCoreExpr env (Cast e co)
    = Cast <$> zonkCoreExpr env e <*> zonkCoToCo env co
zonkCoreExpr env (Tick t e)
    = Tick t <$> zonkCoreExpr env e -- Do we need to zonk in ticks?

zonkCoreExpr env (App e1 e2)
    = App <$> zonkCoreExpr env e1 <*> zonkCoreExpr env e2
zonkCoreExpr env (Lam v e)
    = do { (env1, v') <- zonkCoreBndrX env v
         ; Lam v' <$> zonkCoreExpr env1 e }
zonkCoreExpr env (Let bind e)
    = do (env1, bind') <- zonkCoreBind env bind
         Let bind'<$> zonkCoreExpr env1 e
zonkCoreExpr env (Case scrut b ty alts)
    = do scrut' <- zonkCoreExpr env scrut
         ty' <- zonkTcTypeToTypeX env ty
         b' <- zonkIdBndr env b
         let env1 = extendIdZonkEnv env b'
         alts' <- mapM (zonkCoreAlt env1) alts
         return $ Case scrut' b' ty' alts'

zonkCoreAlt :: ZonkEnv -> CoreAlt -> TcM CoreAlt
zonkCoreAlt env (dc, bndrs, rhs)
    = do (env1, bndrs') <- zonkCoreBndrsX env bndrs
         rhs' <- zonkCoreExpr env1 rhs
         return $ (dc, bndrs', rhs')

zonkCoreBind :: ZonkEnv -> CoreBind -> TcM (ZonkEnv, CoreBind)
zonkCoreBind env (NonRec v e)
    = do v' <- zonkIdBndr env v
         e' <- zonkCoreExpr env e
         let env1 = extendIdZonkEnv env v'
         return (env1, NonRec v' e')
zonkCoreBind env (Rec pairs)
    = do (env1, pairs') <- fixM go
         return (env1, Rec pairs')
  where
    go ~(_, new_pairs) = do
         let env1 = extendIdZonkEnvRec env (map fst new_pairs)
         pairs' <- mapM (zonkCorePair env1) pairs
         return (env1, pairs')

zonkCorePair :: ZonkEnv -> (CoreBndr, CoreExpr) -> TcM (CoreBndr, CoreExpr)
zonkCorePair env (v,e) = (,) <$> zonkIdBndr env v <*> zonkCoreExpr env e

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
zonkEvBindsVar env (CoEvBindsVar {}) = return (env, emptyBag)

zonkEvBinds :: ZonkEnv -> Bag EvBind -> TcM (ZonkEnv, Bag EvBind)
zonkEvBinds env binds
  = {-# SCC "zonkEvBinds" #-}
    fixM (\ ~( _, new_binds) -> do
         { let env1 = extendIdZonkEnvRec env (collect_ev_bndrs new_binds)
         ; binds' <- mapBagM (zonkEvBind env1) binds
         ; return (env1, binds') })
  where
    collect_ev_bndrs :: Bag EvBind -> [EvVar]
    collect_ev_bndrs = foldr add []
    add (EvBind { eb_lhs = var }) vars = var : vars

zonkEvBind :: ZonkEnv -> EvBind -> TcM EvBind
zonkEvBind env bind@(EvBind { eb_lhs = var, eb_rhs = term })
  = do { var'  <- {-# SCC "zonkEvBndr" #-} zonkEvBndr env var

         -- Optimise the common case of Refl coercions
         -- See Note [Optimise coercion zonking]
         -- This has a very big effect on some programs (eg #5030)

       ; term' <- case getEqPredTys_maybe (idType var') of
           Just (r, ty1, ty2) | ty1 `eqType` ty2
                  -> return (evCoercion (mkTcReflCo r ty1))
           _other -> zonkEvTerm env term

       ; return (bind { eb_lhs = var', eb_rhs = term' }) }

{- Note [Optimise coercion zonking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
to a lot of effort to prove Refl!  (Eg when solving  10+3 = 10+3; cf #5030)


************************************************************************
*                                                                      *
                         Zonking types
*                                                                      *
************************************************************************
-}

{- Note [Sharing when zonking to Type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Problem:

    In TcMType.zonkTcTyVar, we short-circuit (Indirect ty) to
    (Indirect zty), see Note [Sharing in zonking] in TcMType. But we
    /can't/ do this when zonking a TcType to a Type (#15552, esp
    comment:3).  Suppose we have

       alpha -> alpha
         where
            alpha is already unified:
             alpha := T{tc-tycon} Int -> Int
         and T is knot-tied

    By "knot-tied" I mean that the occurrence of T is currently a TcTyCon,
    but the global env contains a mapping "T" :-> T{knot-tied-tc}. See
    Note [Type checking recursive type and class declarations] in
    TcTyClsDecls.

    Now we call zonkTcTypeToType on that (alpha -> alpha). If we follow
    the same path as Note [Sharing in zonking] in TcMType, we'll
    update alpha to
       alpha := T{knot-tied-tc} Int -> Int

    But alas, if we encounter alpha for a /second/ time, we end up
    looking at T{knot-tied-tc} and fall into a black hole. The whole
    point of zonkTcTypeToType is that it produces a type full of
    knot-tied tycons, and you must not look at the result!!

    To put it another way (zonkTcTypeToType . zonkTcTypeToType) is not
    the same as zonkTcTypeToType. (If we distinguished TcType from
    Type, this issue would have been a type error!)

Solution: (see #15552 for other variants)

    One possible solution is simply not to do the short-circuiting.
    That has less sharing, but maybe sharing is rare. And indeed,
    that turns out to be viable from a perf point of view

    But the code implements something a bit better

    * ZonkEnv contains ze_meta_tv_env, which maps
          from a MetaTyVar (unification variable)
          to a Type (not a TcType)

    * In zonkTyVarOcc, we check this map to see if we have zonked
      this variable before. If so, use the previous answer; if not
      zonk it, and extend the map.

    * The map is of course stateful, held in a TcRef. (That is unlike
      the treatment of lexically-scoped variables in ze_tv_env and
      ze_id_env.)

    Is the extra work worth it?  Some non-sytematic perf measurements
    suggest that compiler allocation is reduced overall (by 0.5% or so)
    but compile time really doesn't change.
-}

zonkTyVarOcc :: ZonkEnv -> TyVar -> TcM TcType
zonkTyVarOcc env@(ZonkEnv { ze_flexi = flexi
                          , ze_tv_env = tv_env
                          , ze_meta_tv_env = mtv_env_ref }) tv
  | isTcTyVar tv
  = case tcTyVarDetails tv of
      SkolemTv {}    -> lookup_in_tv_env
      RuntimeUnk {}  -> lookup_in_tv_env
      MetaTv { mtv_ref = ref }
        -> do { mtv_env <- readTcRef mtv_env_ref
                -- See Note [Sharing when zonking to Type]
              ; case lookupVarEnv mtv_env tv of
                  Just ty -> return ty
                  Nothing -> do { mtv_details <- readTcRef ref
                                ; zonk_meta mtv_env ref mtv_details } }
  | otherwise
  = lookup_in_tv_env

  where
    lookup_in_tv_env    -- Look up in the env just as we do for Ids
      = case lookupVarEnv tv_env tv of
          Nothing  -> mkTyVarTy <$> updateTyVarKindM (zonkTcTypeToTypeX env) tv
          Just tv' -> return (mkTyVarTy tv')

    zonk_meta mtv_env ref Flexi
      = do { kind <- zonkTcTypeToTypeX env (tyVarKind tv)
           ; ty <- commitFlexi flexi tv kind
           ; writeMetaTyVarRef tv ref ty  -- Belt and braces
           ; finish_meta mtv_env ty }

    zonk_meta mtv_env _ (Indirect ty)
      = do { zty <- zonkTcTypeToTypeX env ty
           ; finish_meta mtv_env zty }

    finish_meta mtv_env ty
      = do { let mtv_env' = extendVarEnv mtv_env tv ty
           ; writeTcRef mtv_env_ref mtv_env'
           ; return ty }

lookupTyVarOcc :: ZonkEnv -> TcTyVar -> Maybe TyVar
lookupTyVarOcc (ZonkEnv { ze_tv_env = tv_env }) tv
  = lookupVarEnv tv_env tv

commitFlexi :: ZonkFlexi -> TcTyVar -> Kind -> TcM Type
-- Only monadic so we can do tc-tracing
commitFlexi flexi tv zonked_kind
  = case flexi of
      SkolemiseFlexi  -> return (mkTyVarTy (mkTyVar name zonked_kind))

      DefaultFlexi
        | isRuntimeRepTy zonked_kind
        -> do { traceTc "Defaulting flexi tyvar to LiftedRep:" (pprTyVar tv)
              ; return liftedRepTy }
        | otherwise
        -> do { traceTc "Defaulting flexi tyvar to Any:" (pprTyVar tv)
              ; return (anyTypeOfKind zonked_kind) }

      RuntimeUnkFlexi
        -> do { traceTc "Defaulting flexi tyvar to RuntimeUnk:" (pprTyVar tv)
              ; return (mkTyVarTy (mkTcTyVar name zonked_kind RuntimeUnk)) }
                        -- This is where RuntimeUnks are born:
                        -- otherwise-unconstrained unification variables are
                        -- turned into RuntimeUnks as they leave the
                        -- typechecker's monad
  where
     name = tyVarName tv

zonkCoVarOcc :: ZonkEnv -> CoVar -> TcM Coercion
zonkCoVarOcc (ZonkEnv { ze_tv_env = tyco_env }) cv
  | Just cv' <- lookupVarEnv tyco_env cv  -- don't look in the knot-tied env
  = return $ mkCoVarCo cv'
  | otherwise
  = do { cv' <- zonkCoVar cv; return (mkCoVarCo cv') }

zonkCoHole :: ZonkEnv -> CoercionHole -> TcM Coercion
zonkCoHole env hole@(CoercionHole { ch_ref = ref, ch_co_var = cv })
  = do { contents <- readTcRef ref
       ; case contents of
           Just co -> do { co' <- zonkCoToCo env co
                         ; checkCoercionHole cv co' }

              -- This next case should happen only in the presence of
              -- (undeferred) type errors. Originally, I put in a panic
              -- here, but that caused too many uses of `failIfErrsM`.
           Nothing -> do { traceTc "Zonking unfilled coercion hole" (ppr hole)
                         ; when debugIsOn $
                           whenNoErrs $
                           MASSERT2( False
                                   , text "Type-correct unfilled coercion hole"
                                     <+> ppr hole )
                         ; cv' <- zonkCoVar cv
                         ; return $ mkCoVarCo cv' } }
                             -- This will be an out-of-scope variable, but keeping
                             -- this as a coercion hole led to #15787

zonk_tycomapper :: TyCoMapper ZonkEnv TcM
zonk_tycomapper = TyCoMapper
  { tcm_tyvar      = zonkTyVarOcc
  , tcm_covar      = zonkCoVarOcc
  , tcm_hole       = zonkCoHole
  , tcm_tycobinder = \env tv _vis -> zonkTyBndrX env tv
  , tcm_tycon      = zonkTcTyConToTyCon }

-- Zonk a TyCon by changing a TcTyCon to a regular TyCon
zonkTcTyConToTyCon :: TcTyCon -> TcM TyCon
zonkTcTyConToTyCon tc
  | isTcTyCon tc = do { thing <- tcLookupGlobalOnly (getName tc)
                      ; case thing of
                          ATyCon real_tc -> return real_tc
                          _              -> pprPanic "zonkTcTyCon" (ppr tc $$ ppr thing) }
  | otherwise    = return tc -- it's already zonked

-- Confused by zonking? See Note [What is zonking?] in TcMType.
zonkTcTypeToType :: TcType -> TcM Type
zonkTcTypeToType ty = initZonkEnv $ \ ze -> zonkTcTypeToTypeX ze ty

zonkTcTypeToTypeX :: ZonkEnv -> TcType -> TcM Type
zonkTcTypeToTypeX = mapType zonk_tycomapper

zonkTcTypesToTypes :: [TcType] -> TcM [Type]
zonkTcTypesToTypes tys = initZonkEnv $ \ ze -> zonkTcTypesToTypesX ze tys

zonkTcTypesToTypesX :: ZonkEnv -> [TcType] -> TcM [Type]
zonkTcTypesToTypesX env tys = mapM (zonkTcTypeToTypeX env) tys

zonkCoToCo :: ZonkEnv -> Coercion -> TcM Coercion
zonkCoToCo = mapCoercion zonk_tycomapper

zonkTcMethInfoToMethInfoX :: ZonkEnv -> TcMethInfo -> TcM MethInfo
zonkTcMethInfoToMethInfoX ze (name, ty, gdm_spec)
  = do { ty' <- zonkTcTypeToTypeX ze ty
       ; gdm_spec' <- zonk_gdm gdm_spec
       ; return (name, ty', gdm_spec') }
  where
    zonk_gdm :: Maybe (DefMethSpec (SrcSpan, TcType))
             -> TcM (Maybe (DefMethSpec (SrcSpan, Type)))
    zonk_gdm Nothing = return Nothing
    zonk_gdm (Just VanillaDM) = return (Just VanillaDM)
    zonk_gdm (Just (GenericDM (loc, ty)))
      = do { ty' <- zonkTcTypeToTypeX ze ty
           ; return (Just (GenericDM (loc, ty'))) }

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
