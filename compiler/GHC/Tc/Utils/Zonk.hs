
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# LANGUAGE LambdaCase #-}

{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1996-1998

-}

-- | Specialisations of the @HsSyn@ syntax for the typechecker
--
-- This module is an extension of @HsSyn@ syntax, for use in the type checker.
module GHC.Tc.Utils.Zonk (
        -- * Other HsSyn functions
        mkHsDictLet, mkHsApp,
        mkHsAppTy, mkHsCaseAlt,
        tcShortCutLit, shortCutLit, hsOverLitName,
        conLikeResTy,

        -- * re-exported from TcMonad
        TcId, TcIdSet,

        -- * Zonking
        -- | For a description of "zonking", see Note [What is zonking?]
        -- in "GHC.Tc.Utils.TcMType"
        zonkTopDecls, zonkTopExpr, zonkTopLExpr,
        zonkTopBndrs,
        ZonkEnv, ZonkFlexi(..), emptyZonkEnv, mkEmptyZonkEnv, initZonkEnv,
        zonkTyVarBindersX, zonkTyVarBinderX,
        zonkTyBndrs, zonkTyBndrsX,
        zonkTcTypeToType,  zonkTcTypeToTypeX,
        zonkTcTypesToTypesX, zonkScaledTcTypesToTypesX,
        zonkTyVarOcc,
        zonkCoToCo,
        zonkEvBinds, zonkTcEvBinds,
        zonkTcMethInfoToMethInfoX,
        lookupTyVarOcc
  ) where

import GHC.Prelude

import GHC.Platform

import GHC.Builtin.Types
import GHC.Builtin.Names

import GHC.Hs

import {-# SOURCE #-} GHC.Tc.Gen.Splice (runTopSplice)
import GHC.Tc.Utils.Monad
import GHC.Tc.TyCl.Build ( TcMethInfo, MethInfo )
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.TcMType
import GHC.Tc.Utils.Env   ( tcLookupGlobalOnly )
import GHC.Tc.Types.Evidence

import GHC.Core.TyCo.Ppr ( pprTyVar )
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.Coercion
import GHC.Core.ConLike
import GHC.Core.DataCon

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Constants (debugIsOn)

import GHC.Core.Multiplicity
import GHC.Core
import GHC.Core.Predicate

import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.TypeEnv
import GHC.Types.SourceText
import GHC.Types.Basic
import GHC.Types.SrcLoc
import GHC.Types.Unique.FM
import GHC.Types.TyThing
import GHC.Driver.Session( getDynFlags, targetPlatform )

import GHC.Data.Maybe
import GHC.Data.Bag

import Control.Monad
import Data.List  ( partition )
import Control.Arrow ( second )

{- *********************************************************************
*                                                                      *
         Short-cuts for overloaded numeric literals
*                                                                      *
********************************************************************* -}

-- Overloaded literals. Here mainly because it uses isIntTy etc

{- Note [Short cut for overloaded literals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A literal like "3" means (fromInteger @ty (dNum :: Num ty) (3::Integer)).
But if we have a list like
  [4,2,3,2,4,4,2]::[Int]
we use a lot of compile time and space generating and solving all those Num
constraints, and generating calls to fromInteger etc.  Better just to cut to
the chase, and cough up an Int literal. Large collections of literals like this
sometimes appear in source files, so it's quite a worthwhile fix.

So we try to take advantage of whatever nearby type information we have,
to short-cut the process for built-in types.  We can do this in two places;

* In the typechecker, when we are about to typecheck the literal.
* If that fails, in the desugarer, once we know the final type.
-}

tcShortCutLit :: HsOverLit GhcRn -> ExpRhoType -> TcM (Maybe (HsOverLit GhcTc))
tcShortCutLit lit@(OverLit { ol_val = val, ol_ext = OverLitRn rebindable _}) exp_res_ty
  | not rebindable
  , Just res_ty <- checkingExpType_maybe exp_res_ty
  = do { dflags <- getDynFlags
       ; let platform = targetPlatform dflags
       ; case shortCutLit platform val res_ty of
            Just expr -> return $ Just $
                         lit { ol_ext = OverLitTc False expr res_ty }
            Nothing   -> return Nothing }
  | otherwise
  = return Nothing

shortCutLit :: Platform -> OverLitVal -> TcType -> Maybe (HsExpr GhcTc)
shortCutLit platform val res_ty
  = case val of
      HsIntegral int_lit    -> go_integral int_lit
      HsFractional frac_lit -> go_fractional frac_lit
      HsIsString s src      -> go_string   s src
  where
    go_integral int@(IL src neg i)
      | isIntTy res_ty  && platformInIntRange  platform i
      = Just (HsLit noAnn (HsInt noExtField int))
      | isWordTy res_ty && platformInWordRange platform i
      = Just (mkLit wordDataCon (HsWordPrim src i))
      | isIntegerTy res_ty
      = Just (HsLit noAnn (HsInteger src i res_ty))
      | otherwise
      = go_fractional (integralFractionalLit neg i)
        -- The 'otherwise' case is important
        -- Consider (3 :: Float).  Syntactically it looks like an IntLit,
        -- so we'll call shortCutIntLit, but of course it's a float
        -- This can make a big difference for programs with a lot of
        -- literals, compiled without -O

    go_fractional f
      | isFloatTy res_ty && valueInRange  = Just (mkLit floatDataCon  (HsFloatPrim noExtField f))
      | isDoubleTy res_ty && valueInRange = Just (mkLit doubleDataCon (HsDoublePrim noExtField f))
      | otherwise                         = Nothing
      where
        valueInRange =
          case f of
            FL { fl_exp = e } -> (-100) <= e && e <= 100
            -- We limit short-cutting Fractional Literals to when their power of 10
            -- is less than 100, which ensures desugaring isn't slow.

    go_string src s
      | isStringTy res_ty = Just (HsLit noAnn (HsString src s))
      | otherwise         = Nothing

mkLit :: DataCon -> HsLit GhcTc -> HsExpr GhcTc
mkLit con lit = HsApp noComments (nlHsDataCon con) (nlHsLit lit)

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

-- Confused by zonking? See Note [What is zonking?] in GHC.Tc.Utils.TcMType.

-- | See Note [The ZonkEnv]
-- Confused by zonking? See Note [What is zonking?] in "GHC.Tc.Utils.TcMType".
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
  It's a way to have a variable that is not a mutable
  unification variable, but doesn't have a binding site
  either.

* NoFlexi: See Note [Error on unconstrained meta-variables]
  in GHC.Tc.Utils.TcMType. This mode will panic on unfilled
  meta-variables.
-}

data ZonkFlexi   -- See Note [Un-unified unification variables]
  = DefaultFlexi    -- Default unbound unification variables to Any
  | SkolemiseFlexi  -- Skolemise unbound unification variables
                    -- See Note [Zonking the LHS of a RULE]
  | RuntimeUnkFlexi -- Used in the GHCi debugger
  | NoFlexi         -- Panic on unfilled meta-variables
                    -- See Note [Error on unconstrained meta-variables]
                    -- in GHC.Tc.Utils.TcMType

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

zonkLIdOcc :: ZonkEnv -> LocatedN TcId -> LocatedN Id
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
  = do Scaled w' ty' <- zonkScaledTcTypeToTypeX env (idScaledType v)
       return (modifyIdInfo (`setLevityInfoWithType` ty') (setIdMult (setIdType v ty') w'))

zonkIdBndrs :: ZonkEnv -> [TcId] -> TcM [Id]
zonkIdBndrs env ids = mapM (zonkIdBndr env) ids

zonkTopBndrs :: [TcId] -> TcM [Id]
zonkTopBndrs ids = initZonkEnv $ \ ze -> zonkIdBndrs ze ids

zonkFieldOcc :: ZonkEnv -> FieldOcc GhcTc -> TcM (FieldOcc GhcTc)
zonkFieldOcc env (FieldOcc sel lbl)
  = fmap ((flip FieldOcc) lbl) $ zonkIdBndr env sel

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
  = updateIdTypeAndMultM ({-# SCC "zonkEvBndr_zonkTcTypeToType" #-} zonkTcTypeToTypeX env) var

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
  = assertPpr (isImmutableTyVar tv) (ppr tv <+> dcolon <+> ppr (tyVarKind tv)) $
    do { ki <- zonkTcTypeToTypeX env (tyVarKind tv)
               -- Internal names tidy up better, for iface files.
       ; let tv' = mkTyVar (tyVarName tv) ki
       ; return (extendTyZonkEnv env tv', tv') }

zonkTyVarBindersX :: ZonkEnv -> [VarBndr TcTyVar vis]
                             -> TcM (ZonkEnv, [VarBndr TyVar vis])
zonkTyVarBindersX = mapAccumLM zonkTyVarBinderX

zonkTyVarBinderX :: ZonkEnv -> VarBndr TcTyVar vis
                            -> TcM (ZonkEnv, VarBndr TyVar vis)
-- Takes a TcTyVar and guarantees to return a TyVar
zonkTyVarBinderX env (Bndr tv vis)
  = do { (env', tv') <- zonkTyBndrX env tv
       ; return (env', Bndr tv' vis) }

zonkTopExpr :: HsExpr GhcTc -> TcM (HsExpr GhcTc)
zonkTopExpr e = initZonkEnv $ \ ze -> zonkExpr ze e

zonkTopLExpr :: LHsExpr GhcTc -> TcM (LHsExpr GhcTc)
zonkTopLExpr e = initZonkEnv $ \ ze -> zonkLExpr ze e

zonkTopDecls :: Bag EvBind
             -> LHsBinds GhcTc
             -> [LRuleDecl GhcTc] -> [LTcSpecPrag]
             -> [LForeignDecl GhcTc]
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
zonkLocalBinds :: ZonkEnv -> HsLocalBinds GhcTc
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
    new_binds <- mapM (wrapLocMA zonk_ip_bind) binds
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

---------------------------------------------
zonkRecMonoBinds :: ZonkEnv -> LHsBinds GhcTc -> TcM (ZonkEnv, LHsBinds GhcTc)
zonkRecMonoBinds env binds
 = fixM (\ ~(_, new_binds) -> do
        { let env1 = extendIdZonkEnvRec env (collectHsBindsBinders CollNoDictBinders new_binds)
        ; binds' <- zonkMonoBinds env1 binds
        ; return (env1, binds') })

---------------------------------------------
zonkMonoBinds :: ZonkEnv -> LHsBinds GhcTc -> TcM (LHsBinds GhcTc)
zonkMonoBinds env binds = mapBagM (zonk_lbind env) binds

zonk_lbind :: ZonkEnv -> LHsBind GhcTc -> TcM (LHsBind GhcTc)
zonk_lbind env = wrapLocMA (zonk_bind env)

zonk_bind :: ZonkEnv -> HsBind GhcTc -> TcM (HsBind GhcTc)
zonk_bind env bind@(PatBind { pat_lhs = pat, pat_rhs = grhss
                            , pat_ext = ty})
  = do  { (_env, new_pat) <- zonkPat env pat            -- Env already extended
        ; new_grhss <- zonkGRHSs env zonkLExpr grhss
        ; new_ty    <- zonkTcTypeToTypeX env ty
        ; return (bind { pat_lhs = new_pat, pat_rhs = new_grhss
                       , pat_ext = new_ty }) }

zonk_bind env (VarBind { var_ext = x
                       , var_id = var, var_rhs = expr })
  = do { new_var  <- zonkIdBndr env var
       ; new_expr <- zonkLExpr env expr
       ; return (VarBind { var_ext = x
                         , var_id = new_var
                         , var_rhs = new_expr }) }

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
  = assert (all isImmutableTyVar tyvars) $
    do { (env0, new_tyvars) <- zonkTyBndrsX env tyvars
       ; (env1, new_evs) <- zonkEvBndrsX env0 evs
       ; (env2, new_ev_binds) <- zonkTcEvBinds_s env1 ev_binds
       ; (new_val_bind, new_exports) <- fixM $ \ ~(new_val_binds, _) ->
         do { let env3 = extendIdZonkEnvRec env2 $
                         collectHsBindsBinders CollNoDictBinders new_val_binds
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
      , (L loc bind@(FunBind { fun_id      = (L mloc mono_id)
                             , fun_matches = ms
                             , fun_ext     = co_fn })) <- lbind
      = do { new_mono_id <- updateIdTypeAndMultM (zonkTcTypeToTypeX env) mono_id
                            -- Specifically /not/ zonkIdBndr; we do not want to
                            -- complain about a representation-polymorphic binder
           ; (env', new_co_fn) <- zonkCoFn env co_fn
           ; new_ms            <- zonkMatchGroup env' zonkLExpr ms
           ; return $ L loc $
             bind { fun_id      = L mloc new_mono_id
                  , fun_matches = new_ms
                  , fun_ext     = new_co_fn } }
      | otherwise
      = zonk_lbind env lbind   -- The normal case

    zonk_export :: ZonkEnv -> ABExport GhcTc -> TcM (ABExport GhcTc)
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

zonk_bind env (PatSynBind x bind@(PSB { psb_id = L loc id
                                      , psb_args = details
                                      , psb_def = lpat
                                      , psb_dir = dir }))
  = do { id' <- zonkIdBndr env id
       ; (env1, lpat') <- zonkPat env lpat
       ; details' <- zonkPatSynDetails env1 details
       ; (_env2, dir') <- zonkPatSynDir env1 dir
       ; return $ PatSynBind x $
                  bind { psb_id = L loc id'
                       , psb_args = details'
                       , psb_def = lpat'
                       , psb_dir = dir' } }

zonkPatSynDetails :: ZonkEnv
                  -> HsPatSynDetails GhcTc
                  -> TcM (HsPatSynDetails GhcTc)
zonkPatSynDetails env (PrefixCon _ as)
  = pure $ PrefixCon noTypeArgs (map (zonkLIdOcc env) as)
zonkPatSynDetails env (InfixCon a1 a2)
  = pure $ InfixCon (zonkLIdOcc env a1) (zonkLIdOcc env a2)
zonkPatSynDetails env (RecCon flds)
  = RecCon <$> mapM (zonkPatSynField env) flds

zonkPatSynField :: ZonkEnv -> RecordPatSynField GhcTc -> TcM (RecordPatSynField GhcTc)
zonkPatSynField env (RecordPatSynField x y) =
    RecordPatSynField <$> zonkFieldOcc env x <*> pure (zonkLIdOcc env y)

zonkPatSynDir :: ZonkEnv -> HsPatSynDir GhcTc
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

zonkMatchGroup :: Anno (GRHS GhcTc (LocatedA (body GhcTc))) ~ SrcAnn NoEpAnns
            => ZonkEnv
            -> (ZonkEnv -> LocatedA (body GhcTc) -> TcM (LocatedA (body GhcTc)))
            -> MatchGroup GhcTc (LocatedA (body GhcTc))
            -> TcM (MatchGroup GhcTc (LocatedA (body GhcTc)))
zonkMatchGroup env zBody (MG { mg_alts = L l ms
                             , mg_ext = MatchGroupTc arg_tys res_ty
                             , mg_origin = origin })
  = do  { ms' <- mapM (zonkMatch env zBody) ms
        ; arg_tys' <- zonkScaledTcTypesToTypesX env arg_tys
        ; res_ty'  <- zonkTcTypeToTypeX env res_ty
        ; return (MG { mg_alts = L l ms'
                     , mg_ext = MatchGroupTc arg_tys' res_ty'
                     , mg_origin = origin }) }

zonkMatch :: Anno (GRHS GhcTc (LocatedA (body GhcTc))) ~ SrcAnn NoEpAnns
          => ZonkEnv
          -> (ZonkEnv -> LocatedA (body GhcTc) -> TcM (LocatedA (body GhcTc)))
          -> LMatch GhcTc (LocatedA (body GhcTc))
          -> TcM (LMatch GhcTc (LocatedA (body GhcTc)))
zonkMatch env zBody (L loc match@(Match { m_pats = pats
                                        , m_grhss = grhss }))
  = do  { (env1, new_pats) <- zonkPats env pats
        ; new_grhss <- zonkGRHSs env1 zBody grhss
        ; return (L loc (match { m_pats = new_pats, m_grhss = new_grhss })) }

-------------------------------------------------------------------------
zonkGRHSs :: Anno (GRHS GhcTc (LocatedA (body GhcTc))) ~ SrcAnn NoEpAnns
          => ZonkEnv
          -> (ZonkEnv -> LocatedA (body GhcTc) -> TcM (LocatedA (body GhcTc)))
          -> GRHSs GhcTc (LocatedA (body GhcTc))
          -> TcM (GRHSs GhcTc (LocatedA (body GhcTc)))

zonkGRHSs env zBody (GRHSs x grhss binds) = do
    (new_env, new_binds) <- zonkLocalBinds env binds
    let
        zonk_grhs (GRHS xx guarded rhs)
          = do (env2, new_guarded) <- zonkStmts new_env zonkLExpr guarded
               new_rhs <- zBody env2 rhs
               return (GRHS xx new_guarded new_rhs)
    new_grhss <- mapM (wrapLocMA zonk_grhs) grhss
    return (GRHSs x new_grhss new_binds)

{-
************************************************************************
*                                                                      *
\subsection[BackSubst-HsExpr]{Running a zonkitution over a TypeCheckedExpr}
*                                                                      *
************************************************************************
-}

zonkLExprs :: ZonkEnv -> [LHsExpr GhcTc] -> TcM [LHsExpr GhcTc]
zonkLExpr  :: ZonkEnv -> LHsExpr GhcTc   -> TcM (LHsExpr GhcTc)
zonkExpr   :: ZonkEnv -> HsExpr GhcTc    -> TcM (HsExpr GhcTc)

zonkLExprs env exprs = mapM (zonkLExpr env) exprs
zonkLExpr  env expr  = wrapLocMA (zonkExpr env) expr

zonkExpr env (HsVar x (L l id))
  = assertPpr (isNothing (isDataConId_maybe id)) (ppr id) $
    return (HsVar x (L l (zonkIdOcc env id)))

zonkExpr env (HsUnboundVar her occ)
  = do her' <- zonk_her her
       return (HsUnboundVar her' occ)
  where
    zonk_her :: HoleExprRef -> TcM HoleExprRef
    zonk_her (HER ref ty u)
      = do updMutVarM ref (zonkEvTerm env)
           ty'  <- zonkTcTypeToTypeX env ty
           return (HER ref ty' u)

zonkExpr env (HsRecSel _ (FieldOcc v occ))
  = return (HsRecSel noExtField (FieldOcc (zonkIdOcc env v) occ))

zonkExpr _ (HsIPVar x _) = dataConCantHappen x

zonkExpr _ (HsOverLabel x _) = dataConCantHappen x

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

zonkExpr env (HsAppType ty e t)
  = do new_e <- zonkLExpr env e
       new_ty <- zonkTcTypeToTypeX env ty
       return (HsAppType new_ty new_e t)
       -- NB: the type is an HsType; can't zonk that!

zonkExpr _ (HsRnBracketOut x _ _) = dataConCantHappen x

zonkExpr env (HsTcBracketOut ty wrap body bs)
  = do wrap' <- traverse zonkQuoteWrap wrap
       bs' <- mapM (zonk_b env) bs
       new_ty <- zonkTcTypeToTypeX env ty
       return (HsTcBracketOut new_ty wrap' body bs')
  where
    zonkQuoteWrap (QuoteWrapper ev ty) = do
        let ev' = zonkIdOcc env ev
        ty' <- zonkTcTypeToTypeX env ty
        return (QuoteWrapper ev' ty')

    zonk_b env' (PendingTcSplice n e) = do e' <- zonkLExpr env' e
                                           return (PendingTcSplice n e')

zonkExpr env (HsSpliceE _ (XSplice (HsSplicedT s))) =
  runTopSplice s >>= zonkExpr env

zonkExpr _ e@(HsSpliceE _ _) = pprPanic "zonkExpr: HsSpliceE" (ppr e)

zonkExpr _ (OpApp x _ _ _) = dataConCantHappen x

zonkExpr env (NegApp x expr op)
  = do (env', new_op) <- zonkSyntaxExpr env op
       new_expr <- zonkLExpr env' expr
       return (NegApp x new_expr new_op)

zonkExpr env (HsPar x lpar e rpar)
  = do new_e <- zonkLExpr env e
       return (HsPar x lpar new_e rpar)

zonkExpr _ (SectionL x _ _) = dataConCantHappen x
zonkExpr _ (SectionR x _ _) = dataConCantHappen x
zonkExpr env (ExplicitTuple x tup_args boxed)
  = do { new_tup_args <- mapM zonk_tup_arg tup_args
       ; return (ExplicitTuple x new_tup_args boxed) }
  where
    zonk_tup_arg (Present x e) = do { e' <- zonkLExpr env e
                                    ; return (Present x e') }
    zonk_tup_arg (Missing t) = do { t' <- zonkScaledTcTypeToTypeX env t
                                  ; return (Missing t') }


zonkExpr env (ExplicitSum args alt arity expr)
  = do new_args <- mapM (zonkTcTypeToTypeX env) args
       new_expr <- zonkLExpr env expr
       return (ExplicitSum new_args alt arity new_expr)

zonkExpr env (HsCase x expr ms)
  = do new_expr <- zonkLExpr env expr
       new_ms <- zonkMatchGroup env zonkLExpr ms
       return (HsCase x new_expr new_ms)

zonkExpr env (HsIf x e1 e2 e3)
  = do new_e1 <- zonkLExpr env e1
       new_e2 <- zonkLExpr env e2
       new_e3 <- zonkLExpr env e3
       return (HsIf x new_e1 new_e2 new_e3)

zonkExpr env (HsMultiIf ty alts)
  = do { alts' <- mapM (wrapLocMA zonk_alt) alts
       ; ty'   <- zonkTcTypeToTypeX env ty
       ; return $ HsMultiIf ty' alts' }
  where zonk_alt (GRHS x guard expr)
          = do { (env', guard') <- zonkStmts env zonkLExpr guard
               ; expr'          <- zonkLExpr env' expr
               ; return $ GRHS x guard' expr' }

zonkExpr env (HsLet x tkLet binds tkIn expr)
  = do (new_env, new_binds) <- zonkLocalBinds env binds
       new_expr <- zonkLExpr new_env expr
       return (HsLet x tkLet new_binds tkIn new_expr)

zonkExpr env (HsDo ty do_or_lc (L l stmts))
  = do (_, new_stmts) <- zonkStmts env zonkLExpr stmts
       new_ty <- zonkTcTypeToTypeX env ty
       return (HsDo new_ty do_or_lc (L l new_stmts))

zonkExpr env (ExplicitList ty exprs)
  = do new_ty <- zonkTcTypeToTypeX env ty
       new_exprs <- zonkLExprs env exprs
       return (ExplicitList new_ty new_exprs)

zonkExpr env expr@(RecordCon { rcon_ext = con_expr, rcon_flds = rbinds })
  = do  { new_con_expr <- zonkExpr env con_expr
        ; new_rbinds   <- zonkRecFields env rbinds
        ; return (expr { rcon_ext  = new_con_expr
                       , rcon_flds = new_rbinds }) }

-- Record updates via dot syntax are replaced by desugared expressions
-- in the renamer. See Note [Rebindable Syntax and HsExpansion]. This
-- is why we match on 'rupd_flds = Left rbinds' here and panic otherwise.
zonkExpr env (RecordUpd { rupd_flds = Left rbinds
                        , rupd_expr = expr
                        , rupd_ext = RecordUpdTc {
                                       rupd_cons = cons
                                     , rupd_in_tys = in_tys
                                     , rupd_out_tys = out_tys
                                     , rupd_wrap = req_wrap }})
  = do  { new_expr    <- zonkLExpr env expr
        ; new_in_tys  <- mapM (zonkTcTypeToTypeX env) in_tys
        ; new_out_tys <- mapM (zonkTcTypeToTypeX env) out_tys
        ; new_rbinds  <- zonkRecUpdFields env rbinds
        ; (_, new_recwrap) <- zonkCoFn env req_wrap
        ; return (
            RecordUpd {
                  rupd_expr = new_expr
                , rupd_flds = Left new_rbinds
                , rupd_ext = RecordUpdTc {
                               rupd_cons = cons
                             , rupd_in_tys = new_in_tys
                             , rupd_out_tys = new_out_tys
                             , rupd_wrap = new_recwrap }}) }
zonkExpr _ (RecordUpd {}) = panic "GHC.Tc.Utils.Zonk: zonkExpr: The impossible happened!"

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

zonkExpr env (XExpr (WrapExpr (HsWrap co_fn expr)))
  = do (env1, new_co_fn) <- zonkCoFn env co_fn
       new_expr <- zonkExpr env1 expr
       return (XExpr (WrapExpr (HsWrap new_co_fn new_expr)))

zonkExpr env (XExpr (ExpansionExpr (HsExpanded a b)))
  = XExpr . ExpansionExpr . HsExpanded a <$> zonkExpr env b

zonkExpr env (XExpr (ConLikeTc con tvs tys))
  = XExpr . ConLikeTc con tvs <$> mapM zonk_scale tys
  where
    zonk_scale (Scaled m ty) = Scaled <$> zonkTcTypeToTypeX env m <*> pure ty
    -- Only the multiplicity can contain unification variables
    -- The tvs come straight from the data-con, and so are strictly redundant
    -- See Wrinkles of Note [Typechecking data constructors] in GHC.Tc.Gen.Head

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
is justified by the use of newUnique in GHC.Tc.Utils.TcMType.instSkolTyCoVarX.)
Now, we can safely just extend one environment.
-}

-- See Note [Skolems in zonkSyntaxExpr]
zonkSyntaxExpr :: ZonkEnv -> SyntaxExpr GhcTc
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

zonkLCmd  :: ZonkEnv -> LHsCmd GhcTc   -> TcM (LHsCmd GhcTc)
zonkCmd   :: ZonkEnv -> HsCmd GhcTc    -> TcM (HsCmd GhcTc)

zonkLCmd  env cmd  = wrapLocMA (zonkCmd env) cmd

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

zonkCmd env (HsCmdPar x lpar c rpar)
  = do new_c <- zonkLCmd env c
       return (HsCmdPar x lpar new_c rpar)

zonkCmd env (HsCmdCase x expr ms)
  = do new_expr <- zonkLExpr env expr
       new_ms <- zonkMatchGroup env zonkLCmd ms
       return (HsCmdCase x new_expr new_ms)

zonkCmd env (HsCmdLamCase x ms)
  = do new_ms <- zonkMatchGroup env zonkLCmd ms
       return (HsCmdLamCase x new_ms)

zonkCmd env (HsCmdIf x eCond ePred cThen cElse)
  = do { (env1, new_eCond) <- zonkSyntaxExpr env eCond
       ; new_ePred <- zonkLExpr env1 ePred
       ; new_cThen <- zonkLCmd env1 cThen
       ; new_cElse <- zonkLCmd env1 cElse
       ; return (HsCmdIf x new_eCond new_ePred new_cThen new_cElse) }

zonkCmd env (HsCmdLet x tkLet binds tkIn cmd)
  = do (new_env, new_binds) <- zonkLocalBinds env binds
       new_cmd <- zonkLCmd new_env cmd
       return (HsCmdLet x tkLet new_binds tkIn new_cmd)

zonkCmd env (HsCmdDo ty (L l stmts))
  = do (_, new_stmts) <- zonkStmts env zonkLCmd stmts
       new_ty <- zonkTcTypeToTypeX env ty
       return (HsCmdDo new_ty (L l new_stmts))



zonkCmdTop :: ZonkEnv -> LHsCmdTop GhcTc -> TcM (LHsCmdTop GhcTc)
zonkCmdTop env cmd = wrapLocMA (zonk_cmd_top env) cmd

zonk_cmd_top :: ZonkEnv -> HsCmdTop GhcTc -> TcM (HsCmdTop GhcTc)
zonk_cmd_top env (HsCmdTop (CmdTopTc stack_tys ty ids) cmd)
  = do new_cmd <- zonkLCmd env cmd
       new_stack_tys <- zonkTcTypeToTypeX env stack_tys
       new_ty <- zonkTcTypeToTypeX env ty
       new_ids <- mapSndM (zonkExpr env) ids

       massert (isLiftedTypeKind (tcTypeKind new_stack_tys))
         -- desugarer assumes that this is not representation-polymorphic...
         -- but indeed it should always be lifted due to the typing
         -- rules for arrows

       return (HsCmdTop (CmdTopTc new_stack_tys new_ty new_ids) new_cmd)

-------------------------------------------------------------------------
zonkCoFn :: ZonkEnv -> HsWrapper -> TcM (ZonkEnv, HsWrapper)
zonkCoFn env WpHole   = return (env, WpHole)
zonkCoFn env (WpCompose c1 c2) = do { (env1, c1') <- zonkCoFn env c1
                                    ; (env2, c2') <- zonkCoFn env1 c2
                                    ; return (env2, WpCompose c1' c2') }
zonkCoFn env (WpFun c1 c2 t1)  = do { (env1, c1') <- zonkCoFn env c1
                                    ; (env2, c2') <- zonkCoFn env1 c2
                                    ; t1'         <- zonkScaledTcTypeToTypeX env2 t1
                                    ; return (env2, WpFun c1' c2' t1') }
zonkCoFn env (WpCast co) = do { co' <- zonkCoToCo env co
                              ; return (env, WpCast co') }
zonkCoFn env (WpEvLam ev)   = do { (env', ev') <- zonkEvBndrX env ev
                                 ; return (env', WpEvLam ev') }
zonkCoFn env (WpEvApp arg)  = do { arg' <- zonkEvTerm env arg
                                 ; return (env, WpEvApp arg') }
zonkCoFn env (WpTyLam tv)   = assert (isImmutableTyVar tv) $
                              do { (env', tv') <- zonkTyBndrX env tv
                                 ; return (env', WpTyLam tv') }
zonkCoFn env (WpTyApp ty)   = do { ty' <- zonkTcTypeToTypeX env ty
                                 ; return (env, WpTyApp ty') }
zonkCoFn env (WpLet bs)     = do { (env1, bs') <- zonkTcEvBinds env bs
                                 ; return (env1, WpLet bs') }
zonkCoFn env (WpMultCoercion co) = do { co' <- zonkCoToCo env co
                                      ; return (env, WpMultCoercion co') }

-------------------------------------------------------------------------
zonkOverLit :: ZonkEnv -> HsOverLit GhcTc -> TcM (HsOverLit GhcTc)
zonkOverLit env lit@(OverLit {ol_ext = x@OverLitTc { ol_witness = e, ol_type = ty } })
  = do  { ty' <- zonkTcTypeToTypeX env ty
        ; e' <- zonkExpr env e
        ; return (lit { ol_ext = x { ol_witness = e'
                                   , ol_type = ty' } }) }

-------------------------------------------------------------------------
zonkArithSeq :: ZonkEnv -> ArithSeqInfo GhcTc -> TcM (ArithSeqInfo GhcTc)

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
zonkStmts :: Anno (StmtLR GhcTc GhcTc (LocatedA (body GhcTc))) ~ SrcSpanAnnA
          => ZonkEnv
          -> (ZonkEnv -> LocatedA (body GhcTc) -> TcM (LocatedA (body GhcTc)))
          -> [LStmt GhcTc (LocatedA (body GhcTc))]
          -> TcM (ZonkEnv, [LStmt GhcTc (LocatedA (body GhcTc))])
zonkStmts env _ []     = return (env, [])
zonkStmts env zBody (s:ss) = do { (env1, s')  <- wrapLocSndMA (zonkStmt env zBody) s
                                ; (env2, ss') <- zonkStmts env1 zBody ss
                                ; return (env2, s' : ss') }

zonkStmt :: Anno (StmtLR GhcTc GhcTc (LocatedA (body GhcTc))) ~ SrcSpanAnnA
         => ZonkEnv
         -> (ZonkEnv -> LocatedA (body GhcTc) -> TcM (LocatedA (body GhcTc)))
         -> Stmt GhcTc (LocatedA (body GhcTc))
         -> TcM (ZonkEnv, Stmt GhcTc (LocatedA (body GhcTc)))
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
    zonk_branch :: ZonkEnv -> ParStmtBlock GhcTc GhcTc
                -> TcM (ParStmtBlock GhcTc GhcTc)
    zonk_branch env1 (ParStmtBlock x stmts bndrs return_op)
       = do { (env2, new_stmts)  <- zonkStmts env1 zonkLExpr stmts
            ; (env3, new_return) <- zonkSyntaxExpr env2 return_op
            ; return (ParStmtBlock x new_stmts (zonkIdOccs env3 bndrs)
                                                                   new_return) }

zonkStmt env zBody (RecStmt { recS_stmts = L _ segStmts, recS_later_ids = lvs
                            , recS_rec_ids = rvs
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
                 RecStmt { recS_stmts = noLocA new_segStmts
                         , recS_later_ids = new_lvs
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

zonkStmt env _ (LetStmt x binds)
  = do (env1, new_binds) <- zonkLocalBinds env binds
       return (env1, LetStmt x new_binds)

zonkStmt env zBody (BindStmt xbs pat body)
  = do  { (env1, new_bind) <- zonkSyntaxExpr env (xbstc_bindOp xbs)
        ; new_w <- zonkTcTypeToTypeX env1 (xbstc_boundResultMult xbs)
        ; new_bind_ty <- zonkTcTypeToTypeX env1 (xbstc_boundResultType xbs)
        ; new_body <- zBody env1 body
        ; (env2, new_pat) <- zonkPat env1 pat
        ; new_fail <- case xbstc_failOp xbs of
            Nothing -> return Nothing
            Just f -> fmap (Just . snd) (zonkSyntaxExpr env1 f)
        ; return ( env2
                 , BindStmt (XBindStmtTc
                              { xbstc_bindOp = new_bind
                              , xbstc_boundResultType = new_bind_ty
                              , xbstc_boundResultMult = new_w
                              , xbstc_failOp = new_fail
                              })
                            new_pat new_body) }

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

    get_pat :: (SyntaxExpr GhcTc, ApplicativeArg GhcTc) -> LPat GhcTc
    get_pat (_, ApplicativeArgOne _ pat _ _) = pat
    get_pat (_, ApplicativeArgMany _ _ _ pat _) = pat

    replace_pat :: LPat GhcTc
                -> (SyntaxExpr GhcTc, ApplicativeArg GhcTc)
                -> (SyntaxExpr GhcTc, ApplicativeArg GhcTc)
    replace_pat pat (op, ApplicativeArgOne fail_op _ a isBody)
      = (op, ApplicativeArgOne fail_op pat a isBody)
    replace_pat pat (op, ApplicativeArgMany x a b _ c)
      = (op, ApplicativeArgMany x a b pat c)

    zonk_args env args
      = do { (env1, new_args_rev) <- zonk_args_rev env (reverse args)
           ; (env2, new_pats)     <- zonkPats env1 (map get_pat args)
           ; return (env2, zipWithEqual "zonkStmt" replace_pat
                                        new_pats (reverse new_args_rev)) }

     -- these need to go backward, because if any operators are higher-rank,
     -- later operators may introduce skolems that are in scope for earlier
     -- arguments
    zonk_args_rev env ((op, arg) : args)
      = do { (env1, new_op)         <- zonkSyntaxExpr env op
           ; new_arg                <- zonk_arg env1 arg
           ; (env2, new_args)       <- zonk_args_rev env1 args
           ; return (env2, (new_op, new_arg) : new_args) }
    zonk_args_rev env [] = return (env, [])

    zonk_arg env (ApplicativeArgOne fail_op pat expr isBody)
      = do { new_expr <- zonkLExpr env expr
           ; new_fail <- forM fail_op $ \old_fail ->
              do { (_, fail') <- zonkSyntaxExpr env old_fail
                 ; return fail'
                 }
           ; return (ApplicativeArgOne new_fail pat new_expr isBody) }
    zonk_arg env (ApplicativeArgMany x stmts ret pat ctxt)
      = do { (env1, new_stmts) <- zonkStmts env zonkLExpr stmts
           ; new_ret           <- zonkExpr env1 ret
           ; return (ApplicativeArgMany x new_stmts new_ret pat ctxt) }

-------------------------------------------------------------------------
zonkRecFields :: ZonkEnv -> HsRecordBinds GhcTc -> TcM (HsRecordBinds GhcTc)
zonkRecFields env (HsRecFields flds dd)
  = do  { flds' <- mapM zonk_rbind flds
        ; return (HsRecFields flds' dd) }
  where
    zonk_rbind (L l fld)
      = do { new_id   <- wrapLocMA (zonkFieldOcc env) (hfbLHS fld)
           ; new_expr <- zonkLExpr env (hfbRHS fld)
           ; return (L l (fld { hfbLHS = new_id
                              , hfbRHS = new_expr })) }

zonkRecUpdFields :: ZonkEnv -> [LHsRecUpdField GhcTc]
                 -> TcM [LHsRecUpdField GhcTc]
zonkRecUpdFields env = mapM zonk_rbind
  where
    zonk_rbind (L l fld)
      = do { new_id   <- wrapLocMA (zonkFieldOcc env) (hsRecUpdFieldOcc fld)
           ; new_expr <- zonkLExpr env (hfbRHS fld)
           ; return (L l (fld { hfbLHS = fmap ambiguousFieldOcc new_id
                              , hfbRHS = new_expr })) }

-------------------------------------------------------------------------
mapIPNameTc :: (a -> TcM b) -> Either (LocatedAn NoEpAnns  HsIPName) a
            -> TcM (Either (LocatedAn NoEpAnns HsIPName) b)
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

zonkPat :: ZonkEnv -> LPat GhcTc -> TcM (ZonkEnv, LPat GhcTc)
-- Extend the environment as we go, because it's possible for one
-- pattern to bind something that is used in another (inside or
-- to the right)
zonkPat env pat = wrapLocSndMA (zonk_pat env) pat

zonk_pat :: ZonkEnv -> Pat GhcTc -> TcM (ZonkEnv, Pat GhcTc)
zonk_pat env (ParPat x lpar p rpar)
  = do  { (env', p') <- zonkPat env p
        ; return (env', ParPat x lpar p' rpar) }

zonk_pat env (WildPat ty)
  = do  { ty' <- zonkTcTypeToTypeX env ty
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

zonk_pat env (ListPat ty pats)
  = do  { ty' <- zonkTcTypeToTypeX env ty
        ; (env', pats') <- zonkPats env pats
        ; return (env', ListPat ty' pats') }

zonk_pat env (TuplePat tys pats boxed)
  = do  { tys' <- mapM (zonkTcTypeToTypeX env) tys
        ; (env', pats') <- zonkPats env pats
        ; return (env', TuplePat tys' pats' boxed) }

zonk_pat env (SumPat tys pat alt arity )
  = do  { tys' <- mapM (zonkTcTypeToTypeX env) tys
        ; (env', pat') <- zonkPat env pat
        ; return (env', SumPat tys' pat' alt arity) }

zonk_pat env p@(ConPat { pat_args = args
                       , pat_con_ext = p'@(ConPatTc
                         { cpt_tvs = tyvars
                         , cpt_dicts = evs
                         , cpt_binds = binds
                         , cpt_wrap = wrapper
                         , cpt_arg_tys = tys
                         })
                       })
  = assert (all isImmutableTyVar tyvars) $
    do  { new_tys <- mapM (zonkTcTypeToTypeX env) tys
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
                   { cpt_arg_tys = new_tys
                   , cpt_tvs = new_tyvars
                   , cpt_dicts = new_evs
                   , cpt_binds = new_binds
                   , cpt_wrap = new_wrapper
                   }
                 }
               )
        }

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
zonk_pat env (XPat ext) = case ext of
  { ExpansionPat orig pat->
    do { (env, pat') <- zonk_pat env pat
       ; return $ (env, XPat $ ExpansionPat orig pat') }
  ; CoPat co_fn pat ty ->
    do { (env', co_fn') <- zonkCoFn env co_fn
       ; (env'', pat') <- zonkPat env' (noLocA pat)
       ; ty' <- zonkTcTypeToTypeX env'' ty
       ; return (env'', XPat $ CoPat co_fn' (unLoc pat') ty')
       }}

zonk_pat _ pat = pprPanic "zonk_pat" (ppr pat)

---------------------------
zonkConStuff :: ZonkEnv -> HsConPatDetails GhcTc
             -> TcM (ZonkEnv, HsConPatDetails GhcTc)
zonkConStuff env (PrefixCon tyargs pats)
  = do  { (env', pats') <- zonkPats env pats
        ; return (env', PrefixCon tyargs pats') }

zonkConStuff env (InfixCon p1 p2)
  = do  { (env1, p1') <- zonkPat env  p1
        ; (env', p2') <- zonkPat env1 p2
        ; return (env', InfixCon p1' p2') }

zonkConStuff env (RecCon (HsRecFields rpats dd))
  = do  { (env', pats') <- zonkPats env (map (hfbRHS . unLoc) rpats)
        ; let rpats' = zipWith (\(L l rp) p' ->
                                  L l (rp { hfbRHS = p' }))
                               rpats pats'
        ; return (env', RecCon (HsRecFields rpats' dd)) }
        -- Field selectors have declared types; hence no zonking

---------------------------
zonkPats :: ZonkEnv -> [LPat GhcTc] -> TcM (ZonkEnv, [LPat GhcTc])
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

zonkForeignExports :: ZonkEnv -> [LForeignDecl GhcTc]
                   -> TcM [LForeignDecl GhcTc]
zonkForeignExports env ls = mapM (wrapLocMA (zonkForeignExport env)) ls

zonkForeignExport :: ZonkEnv -> ForeignDecl GhcTc -> TcM (ForeignDecl GhcTc)
zonkForeignExport env (ForeignExport { fd_name = i, fd_e_ext = co
                                     , fd_fe = spec })
  = return (ForeignExport { fd_name = zonkLIdOcc env i
                          , fd_sig_ty = undefined, fd_e_ext = co
                          , fd_fe = spec })
zonkForeignExport _ for_imp
  = return for_imp     -- Foreign imports don't need zonking

zonkRules :: ZonkEnv -> [LRuleDecl GhcTc] -> TcM [LRuleDecl GhcTc]
zonkRules env rs = mapM (wrapLocMA (zonkRule env)) rs

zonkRule :: ZonkEnv -> RuleDecl GhcTc -> TcM (RuleDecl GhcTc)
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
   zonk_tm_bndr :: ZonkEnv -> LRuleBndr GhcTc -> TcM (ZonkEnv, LRuleBndr GhcTc)
   zonk_tm_bndr env (L l (RuleBndr x (L loc v)))
      = do { (env', v') <- zonk_it env v
           ; return (env', L l (RuleBndr x (L loc v'))) }
   zonk_tm_bndr _ (L _ (RuleBndrSig {})) = panic "zonk_tm_bndr RuleBndrSig"

   zonk_it env v
     | isId v     = do { v' <- zonkIdBndr env v
                       ; return (extendIdZonkEnvRec env [v'], v') }
     | otherwise  = assert (isImmutableTyVar v)
                    zonkTyBndrX env v
                    -- DV: used to be return (env,v) but that is plain
                    -- wrong because we may need to go inside the kind
                    -- of v and zonk there!

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
zonkCoreAlt env (Alt dc bndrs rhs)
    = do (env1, bndrs') <- zonkCoreBndrsX env bndrs
         rhs' <- zonkCoreExpr env1 rhs
         return $ Alt dc bndrs' rhs'

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
zonkEvTypeable env (EvTypeableTrFun tm t1 t2)
  = do { tm' <- zonkEvTerm env tm
       ; t1' <- zonkEvTerm env t1
       ; t2' <- zonkEvTerm env t2
       ; return (EvTypeableTrFun tm' t1' t2') }
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

    In GHC.Tc.Utils.TcMType.zonkTcTyVar, we short-circuit (Indirect ty) to
    (Indirect zty), see Note [Sharing in zonking] in GHC.Tc.Utils.TcMType. But we
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
    GHC.Tc.TyCl.

    Now we call zonkTcTypeToType on that (alpha -> alpha). If we follow
    the same path as Note [Sharing in zonking] in GHC.Tc.Utils.TcMType, we'll
    update alpha to
       alpha := T{knot-tied-tc} Int -> Int

    But alas, if we encounter alpha for a /second/ time, we end up
    looking at T{knot-tied-tc} and fall into a black hole. The whole
    point of zonkTcTypeToType is that it produces a type full of
    knot-tied tycons, and you must not look at the result!!

    To put it another way (zonkTcTypeToType . zonkTcTypeToType) is not
    the same as zonkTcTypeToType. (If we distinguished TcType from
    Type, this issue would have been a type error!)

Solutions: (see #15552 for other variants)

One possible solution is simply not to do the short-circuiting.
That has less sharing, but maybe sharing is rare. And indeed,
that usually turns out to be viable from a perf point of view

But zonkTyVarOcc implements something a bit better

* ZonkEnv contains ze_meta_tv_env, which maps
      from a MetaTyVar (unification variable)
      to a Type (not a TcType)

* In zonkTyVarOcc, we check this map to see if we have zonked
  this variable before. If so, use the previous answer; if not
  zonk it, and extend the map.

* The map is of course stateful, held in a TcRef. (That is unlike
  the treatment of lexically-scoped variables in ze_tv_env and
  ze_id_env.)

* In zonkTyVarOcc we read the TcRef to look up the unification
  variable:
    - if we get a hit we use the zonked result;
    - if not, in zonk_meta we see if the variable is `Indirect ty`,
      zonk that, and update the map (in finish_meta)
  But Nota Bene that the "update map" step must re-read the TcRef
  (or, more precisely, use updTcRef) because the zonking of the
  `Indirect ty` may have added lots of stuff to the map.  See
  #19668 for an example where this made an asymptotic difference!

Is it worth the extra work of carrying ze_meta_tv_env? Some
non-systematic perf measurements suggest that compiler allocation is
reduced overall (by 0.5% or so) but compile time really doesn't
change.  But in some cases it makes a HUGE difference: see test
T9198 and #19668.  So yes, it seems worth it.
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
                                ; zonk_meta ref mtv_details } }
  | otherwise
  = lookup_in_tv_env

  where

    lookup_in_tv_env    -- Look up in the env just as we do for Ids
      = case lookupVarEnv tv_env tv of
          Nothing  -> do
              tv' <- updateTyVarKindM (zonkTcTypeToTypeX env) tv
              return $ mkTyVarTy tv'
          Just tv' -> return (mkTyVarTy tv')

    zonk_meta ref Flexi
      = do { kind <- zonkTcTypeToTypeX env (tyVarKind tv)
           ; ty <- commitFlexi flexi tv kind
           ; writeMetaTyVarRef tv ref ty  -- Belt and braces
           ; finish_meta ty }

    zonk_meta _ (Indirect ty)
      = do { zty <- zonkTcTypeToTypeX env ty
           ; finish_meta zty }

    finish_meta ty
      = do { updTcRef mtv_env_ref (\env -> extendVarEnv env tv ty)
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
        | isMultiplicityTy zonked_kind
        -> do { traceTc "Defaulting flexi tyvar to Many:" (pprTyVar tv)
              ; return manyDataConTy }
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

      NoFlexi -> pprPanic "NoFlexi" (ppr tv <+> dcolon <+> ppr zonked_kind)

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
                           massertPpr False
                                      (text "Type-correct unfilled coercion hole"
                                       <+> ppr hole)
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

-- Confused by zonking? See Note [What is zonking?] in GHC.Tc.Utils.TcMType.
zonkTcTypeToType :: TcType -> TcM Type
zonkTcTypeToType ty = initZonkEnv $ \ ze -> zonkTcTypeToTypeX ze ty

zonkScaledTcTypeToTypeX :: ZonkEnv -> Scaled TcType -> TcM (Scaled TcType)
zonkScaledTcTypeToTypeX env (Scaled m ty) = Scaled <$> zonkTcTypeToTypeX env m
                                                   <*> zonkTcTypeToTypeX env ty

zonkTcTypeToTypeX   :: ZonkEnv -> TcType   -> TcM Type
zonkTcTypesToTypesX :: ZonkEnv -> [TcType] -> TcM [Type]
zonkCoToCo          :: ZonkEnv -> Coercion -> TcM Coercion
(zonkTcTypeToTypeX, zonkTcTypesToTypesX, zonkCoToCo, _)
  = mapTyCoX zonk_tycomapper

zonkScaledTcTypesToTypesX :: ZonkEnv -> [Scaled TcType] -> TcM [Scaled Type]
zonkScaledTcTypesToTypesX env scaled_tys =
   mapM (zonkScaledTcTypeToTypeX env) scaled_tys

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
See also GHC.HsToCore.Binds Note [Free tyvars on rule LHS]

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

* In GHC.HsToCore.Binds, we quantify over it.  See GHC.HsToCore.Binds
  Note [Free tyvars on rule LHS]

Quantifying here is awkward because (a) the data type is big and (b)
finding the free type vars of an expression is necessarily monadic
operation. (consider /\a -> f @ b, where b is side-effected to a)
-}
