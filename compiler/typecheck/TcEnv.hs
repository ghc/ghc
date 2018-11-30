-- (c) The University of Glasgow 2006
{-# LANGUAGE CPP, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}  -- instance MonadThings is necessarily an
                                       -- orphan
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE TypeFamilies #-}

module TcEnv(
        TyThing(..), TcTyThing(..), TcId,

        -- Instance environment, and InstInfo type
        InstInfo(..), iDFunId, pprInstInfoDetails,
        simpleInstInfoClsTy, simpleInstInfoTy, simpleInstInfoTyCon,
        InstBindings(..),

        -- Global environment
        tcExtendGlobalEnv, tcExtendTyConEnv,
        tcExtendGlobalEnvImplicit, setGlobalTypeEnv,
        tcExtendGlobalValEnv,
        tcLookupLocatedGlobal, tcLookupGlobal, tcLookupGlobalOnly,
        tcLookupTyCon, tcLookupClass,
        tcLookupDataCon, tcLookupPatSyn, tcLookupConLike,
        tcLookupLocatedGlobalId, tcLookupLocatedTyCon,
        tcLookupLocatedClass, tcLookupAxiom,
        lookupGlobal, ioLookupDataCon,

        -- Local environment
        tcExtendKindEnv, tcExtendKindEnvList,
        tcExtendTyVarEnv, tcExtendNameTyVarEnv,
        tcExtendLetEnv, tcExtendSigIds, tcExtendRecIds,
        tcExtendIdEnv, tcExtendIdEnv1, tcExtendIdEnv2,
        tcExtendLocalTypeEnv,
        isTypeClosedLetBndr, tcEmitBindingUsage,
        tcCollectingUsage, tcScalingUsage,
        tcExtendBinderStack,

        tcLookup, tcLookupLocated, tcLookupLocalIds,
        tcLookupId, tcLookupIdMaybe, tcLookupTyVar,
        tcLookupLcl_maybe,
        getInLocalScope,
        wrongThingErr, pprBinders,

        tcAddDataFamConPlaceholders, tcAddPatSynPlaceholders,
        getTypeSigNames,
        tcExtendRecEnv,         -- For knot-tying

        -- Tidying
        tcInitTidyEnv, tcInitOpenTidyEnv,

        -- Instances
        tcLookupInstance, tcGetInstEnvs,

        -- Rules
        tcExtendRules,

        -- Defaults
        tcGetDefaultTys,

        -- Global type variables
        tcGetGlobalTyCoVars,

        -- Template Haskell stuff
        checkWellStaged, tcMetaTy, thLevel,
        topIdLvl, isBrackStage,

        -- New Ids
        newDFunName, newDFunName', newFamInstTyConName,
        newFamInstAxiomName,
        mkStableIdFromString, mkStableIdFromName,
        mkWrapperName
  ) where

#include "HsVersions.h"

import GhcPrelude

import HsSyn
import IfaceEnv
import TcRnMonad
import TcMType
import Multiplicity
import UsageEnv
import TcType
import {-# SOURCE #-} TcUnify ( tcSubMult )
import LoadIface
import PrelNames
import TysWiredIn
import Id
import Var
import VarSet
import RdrName
import InstEnv
import DataCon ( DataCon )
import PatSyn  ( PatSyn )
import ConLike
import TyCon
import Type
import CoAxiom
import Class
import Name
import NameSet
import NameEnv
import VarEnv
import HscTypes
import DynFlags
import SrcLoc
import BasicTypes hiding( SuccessFlag(..) )
import Module
import Outputable
import Encoding
import FastString
import ListSetOps
import ErrUtils
import Util
import Maybes( MaybeErr(..), orElse )
import qualified GHC.LanguageExtensions as LangExt

import Data.IORef
import Data.List
import Control.Monad

{- *********************************************************************
*                                                                      *
            An IO interface to looking up globals
*                                                                      *
********************************************************************* -}

lookupGlobal :: HscEnv -> Name -> IO TyThing
-- A variant of lookupGlobal_maybe for the clients which are not
-- interested in recovering from lookup failure and accept panic.
lookupGlobal hsc_env name
  = do  {
          mb_thing <- lookupGlobal_maybe hsc_env name
        ; case mb_thing of
            Succeeded thing -> return thing
            Failed msg      -> pprPanic "lookupGlobal" msg
        }

lookupGlobal_maybe :: HscEnv -> Name -> IO (MaybeErr MsgDoc TyThing)
-- This may look up an Id that one one has previously looked up.
-- If so, we are going to read its interface file, and add its bindings
-- to the ExternalPackageTable.
lookupGlobal_maybe hsc_env name
  = do  {    -- Try local envt
          let mod = icInteractiveModule (hsc_IC hsc_env)
              dflags = hsc_dflags hsc_env
              tcg_semantic_mod = canonicalizeModuleIfHome dflags mod

        ; if nameIsLocalOrFrom tcg_semantic_mod name
              then (return
                (Failed (text "Can't find local name: " <+> ppr name)))
                  -- Internal names can happen in GHCi
              else
           -- Try home package table and external package table
          lookupImported_maybe hsc_env name
        }

lookupImported_maybe :: HscEnv -> Name -> IO (MaybeErr MsgDoc TyThing)
-- Returns (Failed err) if we can't find the interface file for the thing
lookupImported_maybe hsc_env name
  = do  { mb_thing <- lookupTypeHscEnv hsc_env name
        ; case mb_thing of
            Just thing -> return (Succeeded thing)
            Nothing    -> importDecl_maybe hsc_env name
            }

importDecl_maybe :: HscEnv -> Name -> IO (MaybeErr MsgDoc TyThing)
importDecl_maybe hsc_env name
  | Just thing <- wiredInNameTyThing_maybe name
  = do  { when (needWiredInHomeIface thing)
               (initIfaceLoad hsc_env (loadWiredInHomeIface name))
                -- See Note [Loading instances for wired-in things]
        ; return (Succeeded thing) }
  | otherwise
  = initIfaceLoad hsc_env (importDecl name)

ioLookupDataCon :: HscEnv -> Name -> IO DataCon
ioLookupDataCon hsc_env name = do
  mb_thing <- ioLookupDataCon_maybe hsc_env name
  case mb_thing of
    Succeeded thing -> return thing
    Failed msg      -> pprPanic "lookupDataConIO" msg

ioLookupDataCon_maybe :: HscEnv -> Name -> IO (MaybeErr MsgDoc DataCon)
ioLookupDataCon_maybe hsc_env name = do
    thing <- lookupGlobal hsc_env name
    return $ case thing of
        AConLike (RealDataCon con) -> Succeeded con
        _                          -> Failed $
          pprTcTyThingCategory (AGlobal thing) <+> quotes (ppr name) <+>
                text "used as a data constructor"

{-
************************************************************************
*                                                                      *
*                      tcLookupGlobal                                  *
*                                                                      *
************************************************************************

Using the Located versions (eg. tcLookupLocatedGlobal) is preferred,
unless you know that the SrcSpan in the monad is already set to the
span of the Name.
-}


tcLookupLocatedGlobal :: Located Name -> TcM TyThing
-- c.f. IfaceEnvEnv.tcIfaceGlobal
tcLookupLocatedGlobal name
  = addLocM tcLookupGlobal name

tcLookupGlobal :: Name -> TcM TyThing
-- The Name is almost always an ExternalName, but not always
-- In GHCi, we may make command-line bindings (ghci> let x = True)
-- that bind a GlobalId, but with an InternalName
tcLookupGlobal name
  = do  {    -- Try local envt
          env <- getGblEnv
        ; case lookupNameEnv (tcg_type_env env) name of {
                Just thing -> return thing ;
                Nothing    ->

                -- Should it have been in the local envt?
                -- (NB: use semantic mod here, since names never use
                -- identity module, see Note [Identity versus semantic module].)
          if nameIsLocalOrFrom (tcg_semantic_mod env) name
          then notFound name  -- Internal names can happen in GHCi
          else

           -- Try home package table and external package table
    do  { mb_thing <- tcLookupImported_maybe name
        ; case mb_thing of
            Succeeded thing -> return thing
            Failed msg      -> failWithTc msg
        }}}

-- Look up only in this module's global env't. Don't look in imports, etc.
-- Panic if it's not there.
tcLookupGlobalOnly :: Name -> TcM TyThing
tcLookupGlobalOnly name
  = do { env <- getGblEnv
       ; return $ case lookupNameEnv (tcg_type_env env) name of
                    Just thing -> thing
                    Nothing    -> pprPanic "tcLookupGlobalOnly" (ppr name) }

tcLookupDataCon :: Name -> TcM DataCon
tcLookupDataCon name = do
    thing <- tcLookupGlobal name
    case thing of
        AConLike (RealDataCon con) -> return con
        _                          -> wrongThingErr "data constructor" (AGlobal thing) name

tcLookupPatSyn :: Name -> TcM PatSyn
tcLookupPatSyn name = do
    thing <- tcLookupGlobal name
    case thing of
        AConLike (PatSynCon ps) -> return ps
        _                       -> wrongThingErr "pattern synonym" (AGlobal thing) name

tcLookupConLike :: Name -> TcM ConLike
tcLookupConLike name = do
    thing <- tcLookupGlobal name
    case thing of
        AConLike cl -> return cl
        _           -> wrongThingErr "constructor-like thing" (AGlobal thing) name

tcLookupClass :: Name -> TcM Class
tcLookupClass name = do
    thing <- tcLookupGlobal name
    case thing of
        ATyCon tc | Just cls <- tyConClass_maybe tc -> return cls
        _                                           -> wrongThingErr "class" (AGlobal thing) name

tcLookupTyCon :: Name -> TcM TyCon
tcLookupTyCon name = do
    thing <- tcLookupGlobal name
    case thing of
        ATyCon tc -> return tc
        _         -> wrongThingErr "type constructor" (AGlobal thing) name

tcLookupAxiom :: Name -> TcM (CoAxiom Branched)
tcLookupAxiom name = do
    thing <- tcLookupGlobal name
    case thing of
        ACoAxiom ax -> return ax
        _           -> wrongThingErr "axiom" (AGlobal thing) name

tcLookupLocatedGlobalId :: Located Name -> TcM Id
tcLookupLocatedGlobalId = addLocM tcLookupId

tcLookupLocatedClass :: Located Name -> TcM Class
tcLookupLocatedClass = addLocM tcLookupClass

tcLookupLocatedTyCon :: Located Name -> TcM TyCon
tcLookupLocatedTyCon = addLocM tcLookupTyCon

-- Find the instance that exactly matches a type class application.  The class arguments must be precisely
-- the same as in the instance declaration (modulo renaming & casts).
--
tcLookupInstance :: Class -> [Type] -> TcM ClsInst
tcLookupInstance cls tys
  = do { instEnv <- tcGetInstEnvs
       ; case lookupUniqueInstEnv instEnv cls tys of
           Left err             -> failWithTc $ text "Couldn't match instance:" <+> err
           Right (inst, tys)
             | uniqueTyVars tys -> return inst
             | otherwise        -> failWithTc errNotExact
       }
  where
    errNotExact = text "Not an exact match (i.e., some variables get instantiated)"

    uniqueTyVars tys = all isTyVarTy tys
                    && hasNoDups (map (getTyVar "tcLookupInstance") tys)

tcGetInstEnvs :: TcM InstEnvs
-- Gets both the external-package inst-env
-- and the home-pkg inst env (includes module being compiled)
tcGetInstEnvs = do { eps <- getEps
                   ; env <- getGblEnv
                   ; return (InstEnvs { ie_global  = eps_inst_env eps
                                      , ie_local   = tcg_inst_env env
                                      , ie_visible = tcVisibleOrphanMods env }) }

instance MonadThings (IOEnv (Env TcGblEnv TcLclEnv)) where
    lookupThing = tcLookupGlobal

{-
************************************************************************
*                                                                      *
                Extending the global environment
*                                                                      *
************************************************************************
-}

setGlobalTypeEnv :: TcGblEnv -> TypeEnv -> TcM TcGblEnv
-- Use this to update the global type env
-- It updates both  * the normal tcg_type_env field
--                  * the tcg_type_env_var field seen by interface files
setGlobalTypeEnv tcg_env new_type_env
  = do  {     -- Sync the type-envt variable seen by interface files
           writeMutVar (tcg_type_env_var tcg_env) new_type_env
         ; return (tcg_env { tcg_type_env = new_type_env }) }


tcExtendGlobalEnvImplicit :: [TyThing] -> TcM r -> TcM r
  -- Just extend the global environment with some TyThings
  -- Do not extend tcg_tcs, tcg_patsyns etc
tcExtendGlobalEnvImplicit things thing_inside
   = do { tcg_env <- getGblEnv
        ; let ge'  = extendTypeEnvList (tcg_type_env tcg_env) things
        ; tcg_env' <- setGlobalTypeEnv tcg_env ge'
        ; setGblEnv tcg_env' thing_inside }

tcExtendGlobalEnv :: [TyThing] -> TcM r -> TcM r
  -- Given a mixture of Ids, TyCons, Classes, all defined in the
  -- module being compiled, extend the global environment
tcExtendGlobalEnv things thing_inside
  = do { env <- getGblEnv
       ; let env' = env { tcg_tcs = [tc | ATyCon tc <- things] ++ tcg_tcs env,
                          tcg_patsyns = [ps | AConLike (PatSynCon ps) <- things] ++ tcg_patsyns env }
       ; setGblEnv env' $
            tcExtendGlobalEnvImplicit things thing_inside
       }

tcExtendTyConEnv :: [TyCon] -> TcM r -> TcM r
  -- Given a mixture of Ids, TyCons, Classes, all defined in the
  -- module being compiled, extend the global environment
tcExtendTyConEnv tycons thing_inside
  = do { env <- getGblEnv
       ; let env' = env { tcg_tcs = tycons ++ tcg_tcs env }
       ; setGblEnv env' $
         tcExtendGlobalEnvImplicit (map ATyCon tycons) thing_inside
       }

tcExtendGlobalValEnv :: [Id] -> TcM a -> TcM a
  -- Same deal as tcExtendGlobalEnv, but for Ids
tcExtendGlobalValEnv ids thing_inside
  = tcExtendGlobalEnvImplicit [AnId id | id <- ids] thing_inside

tcExtendRecEnv :: [(Name,TyThing)] -> TcM r -> TcM r
-- Extend the global environments for the type/class knot tying game
-- Just like tcExtendGlobalEnv, except the argument is a list of pairs
tcExtendRecEnv gbl_stuff thing_inside
 = do  { tcg_env <- getGblEnv
       ; let ge' = extendNameEnvList (tcg_type_env tcg_env) gbl_stuff
       ; tcg_env' <- setGlobalTypeEnv tcg_env ge'
       ; setGblEnv tcg_env' thing_inside }

{-
************************************************************************
*                                                                      *
\subsection{The local environment}
*                                                                      *
************************************************************************
-}

tcLookupLocated :: Located Name -> TcM TcTyThing
tcLookupLocated = addLocM tcLookup

tcLookupLcl_maybe :: Name -> TcM (Maybe TcTyThing)
tcLookupLcl_maybe name
  = do { local_env <- getLclTypeEnv
       ; return (fmap scaledThing $ lookupNameEnv local_env name) }

tcLookup :: Name -> TcM TcTyThing
tcLookup name = do
    local_env <- getLclTypeEnv
    case lookupNameEnv local_env name of
        Just thing -> return (scaledThing thing)
        Nothing    -> AGlobal <$> tcLookupGlobal name

tcLookupTyVar :: Name -> TcM TcTyVar
tcLookupTyVar name
  = do { thing <- tcLookup name
       ; case thing of
           ATyVar _ tv -> return tv
           _           -> pprPanic "tcLookupTyVar" (ppr name) }

tcLookupId :: Name -> TcM Id
-- Used when we aren't interested in the binding level, nor refinement.
-- The "no refinement" part means that we return the un-refined Id regardless
--
-- The Id is never a DataCon. (Why does that matter? see TcExpr.tcId)
tcLookupId name = do
    thing <- tcLookupIdMaybe name
    case thing of
        Just id -> return id
        _       -> pprPanic "tcLookupId" (ppr name)

tcLookupIdMaybe :: Name -> TcM (Maybe Id)
tcLookupIdMaybe name
  = do { thing <- tcLookup name
       ; case thing of
           ATcId { tct_id = id} -> return $ Just id
           AGlobal (AnId id)    -> return $ Just id
           _                    -> return Nothing }

tcLookupLocalIds :: [Name] -> TcM [TcId]
-- We expect the variables to all be bound, and all at
-- the same level as the lookup.  Only used in one place...
tcLookupLocalIds ns
  = do { env <- getLclEnv
       ; return (map (lookup (tcl_env env)) ns) }
  where
    lookup lenv name
        = case lookupNameEnv lenv name of
                Just (Scaled { scaledThing = (ATcId { tct_id = id })}) ->  id
                _ -> pprPanic "tcLookupLocalIds" (ppr name)

getInLocalScope :: TcM (Name -> Bool)
getInLocalScope = do { lcl_env <- getLclTypeEnv
                     ; return (`elemNameEnv` lcl_env) }

tcExtendKindEnvList :: [(Name, Scaled TcTyThing)] -> TcM r -> TcM r
-- Used only during kind checking, for TcThings that are
--      ATcTyCon or APromotionErr
-- No need to update the global tyvars, or tcl_th_bndrs, or tcl_rdr
tcExtendKindEnvList things thing_inside
  = do { traceTc "tcExtendKindEnvList" (ppr things)
       ; updLclEnv upd_env thing_inside }
  where
    upd_env env = env { tcl_env = extendNameEnvList (tcl_env env) things }

tcExtendKindEnv :: NameEnv (Scaled TcTyThing) -> TcM r -> TcM r
-- A variant of tcExtendKindEvnList
tcExtendKindEnv extra_env thing_inside
  = do { traceTc "tcExtendKindEnv" (ppr extra_env)
       ; updLclEnv upd_env thing_inside }
  where
    upd_env env = env { tcl_env = tcl_env env `plusNameEnv` extra_env }

-----------------------
-- Scoped type and kind variables
tcExtendTyVarEnv :: [Scaled TyVar] -> TcM r -> TcM r
tcExtendTyVarEnv tvs thing_inside
  = tcExtendNameTyVarEnv (mkTyVarNamePairs tvs) thing_inside

tcExtendNameTyVarEnv :: [(Name,Scaled TcTyVar)] -> TcM r -> TcM r
tcExtendNameTyVarEnv binds thing_inside
  -- this should be used only for explicitly mentioned scoped variables.
  -- thus, no coercion variables
  = do { tc_extend_local_env NotTopLevel
                    [(name, ATyVar name <$> wtv) | (name, wtv) <- binds] $
         tcExtendBinderStack tv_binds $
         thing_inside }
  where
    tv_binds :: [TcBinder]
    tv_binds = [TcTvBndr name tv | (name, Scaled _ tv) <- binds]

isTypeClosedLetBndr :: Id -> Bool
-- See Note [Bindings with closed types] in TcRnTypes
isTypeClosedLetBndr = noFreeVarsOfType . idType

tcExtendRecIds :: [(Name, Scaled TcId)] -> TcM a -> TcM a
-- Used for binding the recurive uses of Ids in a binding
-- both top-level value bindings and and nested let/where-bindings
-- Does not extend the TcBinderStack
tcExtendRecIds pairs thing_inside
  = tc_extend_local_env NotTopLevel
          [ (name, (\let_id -> ATcId { tct_id   = let_id
                         , tct_info = NonClosedLet emptyNameSet False }) <$> wlet_id)
          | (name, wlet_id) <- pairs ] $
    thing_inside

tcExtendSigIds :: TopLevelFlag -> [Scaled TcId] -> TcM a -> TcM a
-- Used for binding the Ids that have a complete user type signature
-- Does not extend the TcBinderStack
tcExtendSigIds top_lvl sig_ids thing_inside
  = tc_extend_local_env top_lvl
          [ (idName (scaledThing wid), (\id -> ATcId { tct_id   = id
                                                       , tct_info = info }) <$> wid)
          | wid <- sig_ids
          , let closed = isTypeClosedLetBndr (scaledThing wid)
                info   = NonClosedLet emptyNameSet closed ]
     thing_inside


tcExtendLetEnv :: TopLevelFlag -> TcSigFun -> IsGroupClosed
                  -> [Scaled TcId] -> TcM a -> TcM a
-- Used for both top-level value bindings and and nested let/where-bindings
-- Adds to the TcBinderStack too
tcExtendLetEnv top_lvl sig_fn (IsGroupClosed fvs fv_type_closed)
               ids thing_inside
  = tcExtendBinderStack [TcIdBndr id top_lvl | Scaled _ id <- ids] $
    tc_extend_local_env top_lvl
          [ (idName (scaledThing wid), (\id -> ATcId { tct_id   = id
                                                       , tct_info = mk_tct_info id }) <$> wid)
          | wid <- ids ]
    thing_inside
  where
    mk_tct_info id
      | type_closed && isEmptyNameSet rhs_fvs = ClosedLet
      | otherwise                             = NonClosedLet rhs_fvs type_closed
      where
        name        = idName id
        rhs_fvs     = lookupNameEnv fvs name `orElse` emptyNameSet
        type_closed = isTypeClosedLetBndr id &&
                      (fv_type_closed || hasCompleteSig sig_fn name)

tcExtendIdEnv :: [Scaled TcId] -> TcM a -> TcM a
-- For lambda-bound and case-bound Ids
-- Extends the TcBinderStack as well
tcExtendIdEnv ids thing_inside
  = tcExtendIdEnv2 [(idName (scaledThing id), id) | id <- ids] thing_inside

tcExtendIdEnv1 :: Name -> Scaled TcId -> TcM a -> TcM a
-- Exactly like tcExtendIdEnv2, but for a single (name,id) pair
tcExtendIdEnv1 name id thing_inside
  = tcExtendIdEnv2 [(name,id)] thing_inside

tcExtendIdEnv2 :: [(Name,Scaled TcId)] -> TcM a -> TcM a
tcExtendIdEnv2 names_w_ids thing_inside
  = tcExtendBinderStack [ TcIdBndr mono_id NotTopLevel
                    | (_,Scaled _ mono_id) <- names_w_ids ] $
    do  { tc_extend_local_env NotTopLevel
                              [ (name, Scaled w (ATcId { tct_id = id
                                             , tct_info = NotLetBound }))
                              | (name,Scaled w id) <- names_w_ids] $
          thing_inside }

tc_extend_local_env :: TopLevelFlag -> [(Name, Scaled TcTyThing)]
                    -> TcM a -> TcM a
tc_extend_local_env top_lvl extra_env thing_inside
-- Precondition: the argument list extra_env has TcTyThings
--               that ATcId or ATyVar, but nothing else
--
-- Invariant: the ATcIds are fully zonked. Reasons:
--      (a) The kinds of the forall'd type variables are defaulted
--          (see Kind.defaultKind, done in skolemiseQuantifiedTyVar)
--      (b) There are no via-Indirect occurrences of the bound variables
--          in the types, because instantiation does not look through such things
--      (c) The call to tyCoVarsOfTypes is ok without looking through refs

-- The second argument of type TyVarSet is a set of type variables
-- that are bound together with extra_env and should not be regarded
-- as free in the types of extra_env.
  = do  { traceTc "tc_extend_local_env" (ppr extra_env)
        ; env0 <- getLclEnv
        ; env1 <- tcExtendLocalTypeEnv env0 extra_env
        ; stage <- getStage
        ; let env2 = extend_local_env (top_lvl, thLevel stage) extra_env env1
        ; (local_usage,result) <- setLclEnv env2 (tcCollectingUsage thing_inside)
        ; check_then_add_usage local_usage
        ; return result }
  where
    extend_local_env :: (TopLevelFlag, ThLevel) -> [(Name, Scaled TcTyThing)] -> TcLclEnv -> TcLclEnv
    -- Extend the local LocalRdrEnv and Template Haskell staging env simultaneously
    -- Reason for extending LocalRdrEnv: after running a TH splice we need
    -- to do renaming.
    extend_local_env thlvl pairs env@(TcLclEnv { tcl_rdr = rdr_env
                                               , tcl_th_bndrs = th_bndrs })
      = env { tcl_rdr      = extendLocalRdrEnvList rdr_env
                                [ n | (n, _) <- pairs, isInternalName n ]
                                -- The LocalRdrEnv contains only non-top-level names
                                -- (GlobalRdrEnv handles the top level)
            , tcl_th_bndrs = extendNameEnvList th_bndrs  -- We only track Ids in tcl_th_bndrs
                                 [(n, thlvl) | (n, Scaled _ ATcId {}) <- pairs] }

    check_then_add_usage :: UsageEnv -> TcM ()
    -- Checks that the usage of the newly introduced binders is compatible with
    -- their multiplicity. If so, combines the usage of non-new binders to |uenv|
    check_then_add_usage u0
      = do { uok <- foldM (\u (x,w_) -> check_binder (scaledMult w_) x u) u0 extra_env
           ; env <- getLclEnv
           ; let usage = tcl_usage env
           ; updTcRef usage (addUE uok) }

    check_binder :: Mult -> Name -> UsageEnv -> TcM UsageEnv
    check_binder w x uenv = do
      let actual_w = lookupUE uenv x
      traceTc "check_binder" (ppr w $$ ppr actual_w)
      case submultMaybe actual_w w of
        Smaller -> return ()
        Unknown -> tcSubMult actual_w w
        Larger  ->
          addErrTc $ text "Couldn't match expected multiplicity" <+> quotes (ppr w) <+>
                     text "of variable" <+> quotes (ppr x) <+>
                     text "with actual multiplicity" <+> quotes (ppr actual_w)
          -- In case of error, recover by pretending that the multiplicity usage was correct
      return $ deleteUE uenv x



tcExtendLocalTypeEnv :: TcLclEnv -> [(Name, Scaled TcTyThing)] -> TcM TcLclEnv
tcExtendLocalTypeEnv lcl_env@(TcLclEnv { tcl_env = lcl_type_env }) tc_ty_things
  | isEmptyVarSet extra_tvs
  = return (lcl_env { tcl_env = extendNameEnvList lcl_type_env tc_ty_things })
  | otherwise
  = do { global_tvs <- readMutVar (tcl_tyvars lcl_env)
       ; new_g_var  <- newMutVar (global_tvs `unionVarSet` extra_tvs)
       ; return (lcl_env { tcl_tyvars = new_g_var
                         , tcl_env = extendNameEnvList lcl_type_env tc_ty_things } ) }
  where
    extra_tvs = foldr get_tvs emptyVarSet tc_ty_things

    get_tvs (_, Scaled _ (ATcId { tct_id = id, tct_info = closed })) tvs
      = case closed of
          ClosedLet -> ASSERT2( is_closed_type, ppr id $$ ppr (idType id) )
                       tvs
          _other    -> tvs `unionVarSet` id_tvs
        where
           id_ty          = idType id
           id_tvs         = tyCoVarsOfType id_ty
           id_co_tvs      = closeOverKinds (coVarsOfType id_ty)
           is_closed_type = not (anyVarSet isTyVar (id_tvs `minusVarSet` id_co_tvs))
           -- We only care about being closed wrt /type/ variables
           -- E.g. a top-level binding might have a type like
           --          foo :: t |> co
           -- where co :: * ~ *
           -- or some other as-yet-unsolved kind coercion

    get_tvs (_, Scaled _ (ATyVar _ tv)) tvs          -- See Note [Global TyVars]
      = tvs `unionVarSet` tyCoVarsOfType (tyVarKind tv) `extendVarSet` tv

    get_tvs (_, Scaled _ (ATcTyCon tc)) tvs = tvs `unionVarSet` tyCoVarsOfType (tyConKind tc)

    get_tvs (_, Scaled _ (AGlobal {}))       tvs = tvs
    get_tvs (_, Scaled _ (APromotionErr {})) tvs = tvs

        -- Note [Global TyVars]
        -- It's important to add the in-scope tyvars to the global tyvar set
        -- as well.  Consider
        --      f (_::r) = let g y = y::r in ...
        -- Here, g mustn't be generalised.  This is also important during
        -- class and instance decls, when we mustn't generalise the class tyvars
        -- when typechecking the methods.
        --
        -- Nor must we generalise g over any kind variables free in r's kind

-- | @tcCollectingUsage thing_inside@ runs @thing_inside@ and returns the usage
-- information which was collected as part of the execution of
-- @thing_inside@. Careful: @tcCollectingUsage thing_inside@ itself does not
-- report any usage information, it's up to the programmer to incorporate the
-- returned usage information into the larger context appropriately.
tcCollectingUsage :: TcM a -> TcM (UsageEnv,a)
tcCollectingUsage thing_inside
  = do { env0 <- getLclEnv
       ; (local_usage_ref,env1) <- push_fresh_usage env0
       ; result <- setLclEnv env1 thing_inside
       ; local_usage <- readTcRef local_usage_ref
       ; return (local_usage,result) }
  where
    push_fresh_usage :: TcLclEnv -> TcM (TcRef UsageEnv,TcLclEnv)
    push_fresh_usage env
      = do { usage <- newTcRef zeroUE
           ; return ( usage , env { tcl_usage = usage } ) }

-- | @tcScalingUsage mult thing_inside@ runs @thing_inside@ and scales all the
-- usage information by @mult@.
tcScalingUsage :: Mult -> TcM a -> TcM a
tcScalingUsage mult thing_inside
  = do { (usage, result) <- tcCollectingUsage thing_inside
       ; tcEmitBindingUsage $ scaleUE mult usage
       ; return result }

{- *********************************************************************
*                                                                      *
             The TcBinderStack
*                                                                      *
********************************************************************* -}

tcExtendBinderStack :: [TcBinder] -> TcM a -> TcM a
tcExtendBinderStack bndrs thing_inside
  = do { traceTc "tcExtendBinderStack" (ppr bndrs)
       ; updLclEnv (\env -> env { tcl_bndrs = bndrs ++ tcl_bndrs env })
                   thing_inside }

tcEmitBindingUsage :: UsageEnv -> TcM ()
tcEmitBindingUsage ue
  = do { lcl_env <- getLclEnv
       ; let usage = tcl_usage lcl_env
       ; updTcRef usage (addUE ue) }

tcInitTidyEnv :: TcM TidyEnv
-- We initialise the "tidy-env", used for tidying types before printing,
-- by building a reverse map from the in-scope type variables to the
-- OccName that the programmer originally used for them
tcInitTidyEnv
  = do  { lcl_env <- getLclEnv
        ; go emptyTidyEnv (tcl_bndrs lcl_env) }
  where
    go (env, subst) []
      = return (env, subst)
    go (env, subst) (b : bs)
      | TcTvBndr name tyvar <- b
       = do { let (env', occ') = tidyOccName env (nameOccName name)
                  name'  = tidyNameOcc name occ'
                  tyvar1 = setTyVarName tyvar name'
            ; tyvar2 <- zonkTcTyVarToTyVar tyvar1
              -- Be sure to zonk here!  Tidying applies to zonked
              -- types, so if we don't zonk we may create an
              -- ill-kinded type (Trac #14175)
            ; go (env', extendVarEnv subst tyvar tyvar2) bs }
      | otherwise
      = go (env, subst) bs

-- | Get a 'TidyEnv' that includes mappings for all vars free in the given
-- type. Useful when tidying open types.
tcInitOpenTidyEnv :: [TyCoVar] -> TcM TidyEnv
tcInitOpenTidyEnv tvs
  = do { env1 <- tcInitTidyEnv
       ; let env2 = tidyFreeTyCoVars env1 tvs
       ; return env2 }



{- *********************************************************************
*                                                                      *
             Adding placeholders
*                                                                      *
********************************************************************* -}

tcAddDataFamConPlaceholders :: [LInstDecl GhcRn] -> TcM a -> TcM a
-- See Note [AFamDataCon: not promoting data family constructors]
tcAddDataFamConPlaceholders inst_decls thing_inside
  = tcExtendKindEnvList [ (con, Scaled Omega (APromotionErr FamDataConPE))
                        | lid <- inst_decls, con <- get_cons lid ]
      thing_inside
      -- Note [AFamDataCon: not promoting data family constructors]
  where
    -- get_cons extracts the *constructor* bindings of the declaration
    get_cons :: LInstDecl GhcRn -> [Name]
    get_cons (L _ (TyFamInstD {}))                     = []
    get_cons (L _ (DataFamInstD { dfid_inst = fid }))  = get_fi_cons fid
    get_cons (L _ (ClsInstD { cid_inst = ClsInstDecl { cid_datafam_insts = fids } }))
      = concatMap (get_fi_cons . unLoc) fids
    get_cons (L _ (ClsInstD _ (XClsInstDecl _))) = panic "get_cons"
    get_cons (L _ (XInstDecl _)) = panic "get_cons"

    get_fi_cons :: DataFamInstDecl GhcRn -> [Name]
    get_fi_cons (DataFamInstDecl { dfid_eqn = HsIB { hsib_body =
                  FamEqn { feqn_rhs = HsDataDefn { dd_cons = cons } }}})
      = map unLoc $ concatMap (getConNames . unLoc) cons
    get_fi_cons (DataFamInstDecl { dfid_eqn = HsIB { hsib_body =
                  FamEqn { feqn_rhs = XHsDataDefn _ }}})
      = panic "get_fi_cons"
    get_fi_cons (DataFamInstDecl (HsIB _ (XFamEqn _))) = panic "get_fi_cons"
    get_fi_cons (DataFamInstDecl (XHsImplicitBndrs _)) = panic "get_fi_cons"


tcAddPatSynPlaceholders :: [PatSynBind GhcRn GhcRn] -> TcM a -> TcM a
-- See Note [Don't promote pattern synonyms]
tcAddPatSynPlaceholders pat_syns thing_inside
  = tcExtendKindEnvList [ (name, Scaled Omega (APromotionErr PatSynPE))
                        | PSB{ psb_id = L _ name } <- pat_syns ]
       thing_inside

getTypeSigNames :: [LSig GhcRn] -> NameSet
-- Get the names that have a user type sig
getTypeSigNames sigs
  = foldr get_type_sig emptyNameSet sigs
  where
    get_type_sig :: LSig GhcRn -> NameSet -> NameSet
    get_type_sig sig ns =
      case sig of
        L _ (TypeSig _ names _) -> extendNameSetList ns (map unLoc names)
        L _ (PatSynSig _ names _) -> extendNameSetList ns (map unLoc names)
        _ -> ns


{- Note [AFamDataCon: not promoting data family constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data family T a
  data instance T Int = MkT
  data Proxy (a :: k)
  data S = MkS (Proxy 'MkT)

Is it ok to use the promoted data family instance constructor 'MkT' in
the data declaration for S (where both declarations live in the same module)?
No, we don't allow this. It *might* make sense, but at least it would mean that
we'd have to interleave typechecking instances and data types, whereas at
present we do data types *then* instances.

So to check for this we put in the TcLclEnv a binding for all the family
constructors, bound to AFamDataCon, so that if we trip over 'MkT' when
type checking 'S' we'll produce a decent error message.

Trac #12088 describes this limitation. Of course, when MkT and S live in
different modules then all is well.

Note [Don't promote pattern synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We never promote pattern synonyms.

Consider this (Trac #11265):
  pattern A = True
  instance Eq A
We want a civilised error message from the occurrence of 'A'
in the instance, yet 'A' really has not yet been type checked.

Similarly (Trac #9161)
  {-# LANGUAGE PatternSynonyms, DataKinds #-}
  pattern A = ()
  b :: A
  b = undefined
Here, the type signature for b mentions A.  But A is a pattern
synonym, which is typechecked as part of a group of bindings (for very
good reasons; a view pattern in the RHS may mention a value binding).
It is entirely reasonable to reject this, but to do so we need A to be
in the kind environment when kind-checking the signature for B.

Hence tcAddPatSynPlaceholers adds a binding
    A -> APromotionErr PatSynPE
to the environment. Then TcHsType.tcTyVar will find A in the kind
environment, and will give a 'wrongThingErr' as a result.  But the
lookup of A won't fail.


************************************************************************
*                                                                      *
\subsection{Rules}
*                                                                      *
************************************************************************
-}

tcExtendRules :: [LRuleDecl GhcTc] -> TcM a -> TcM a
        -- Just pop the new rules into the EPS and envt resp
        -- All the rules come from an interface file, not source
        -- Nevertheless, some may be for this module, if we read
        -- its interface instead of its source code
tcExtendRules lcl_rules thing_inside
 = do { env <- getGblEnv
      ; let
          env' = env { tcg_rules = lcl_rules ++ tcg_rules env }
      ; setGblEnv env' thing_inside }

{-
************************************************************************
*                                                                      *
                Meta level
*                                                                      *
************************************************************************
-}

checkWellStaged :: SDoc         -- What the stage check is for
                -> ThLevel      -- Binding level (increases inside brackets)
                -> ThLevel      -- Use stage
                -> TcM ()       -- Fail if badly staged, adding an error
checkWellStaged pp_thing bind_lvl use_lvl
  | use_lvl >= bind_lvl         -- OK! Used later than bound
  = return ()                   -- E.g.  \x -> [| $(f x) |]

  | bind_lvl == outerLevel      -- GHC restriction on top level splices
  = stageRestrictionError pp_thing

  | otherwise                   -- Badly staged
  = failWithTc $                -- E.g.  \x -> $(f x)
    text "Stage error:" <+> pp_thing <+>
        hsep   [text "is bound at stage" <+> ppr bind_lvl,
                text "but used at stage" <+> ppr use_lvl]

stageRestrictionError :: SDoc -> TcM a
stageRestrictionError pp_thing
  = failWithTc $
    sep [ text "GHC stage restriction:"
        , nest 2 (vcat [ pp_thing <+> text "is used in a top-level splice, quasi-quote, or annotation,"
                       , text "and must be imported, not defined locally"])]

topIdLvl :: Id -> ThLevel
-- Globals may either be imported, or may be from an earlier "chunk"
-- (separated by declaration splices) of this module.  The former
--  *can* be used inside a top-level splice, but the latter cannot.
-- Hence we give the former impLevel, but the latter topLevel
-- E.g. this is bad:
--      x = [| foo |]
--      $( f x )
-- By the time we are prcessing the $(f x), the binding for "x"
-- will be in the global env, not the local one.
topIdLvl id | isLocalId id = outerLevel
            | otherwise    = impLevel

tcMetaTy :: Name -> TcM Type
-- Given the name of a Template Haskell data type,
-- return the type
-- E.g. given the name "Expr" return the type "Expr"
tcMetaTy tc_name = do
    t <- tcLookupTyCon tc_name
    return (mkTyConApp t [])

isBrackStage :: ThStage -> Bool
isBrackStage (Brack {}) = True
isBrackStage _other     = False

{-
************************************************************************
*                                                                      *
                 getDefaultTys
*                                                                      *
************************************************************************
-}

tcGetDefaultTys :: TcM ([Type], -- Default types
                        (Bool,  -- True <=> Use overloaded strings
                         Bool)) -- True <=> Use extended defaulting rules
tcGetDefaultTys
  = do  { dflags <- getDynFlags
        ; let ovl_strings = xopt LangExt.OverloadedStrings dflags
              extended_defaults = xopt LangExt.ExtendedDefaultRules dflags
                                        -- See also Trac #1974
              flags = (ovl_strings, extended_defaults)

        ; mb_defaults <- getDeclaredDefaultTys
        ; case mb_defaults of {
           Just tys -> return (tys, flags) ;
                                -- User-supplied defaults
           Nothing  -> do

        -- No use-supplied default
        -- Use [Integer, Double], plus modifications
        { integer_ty <- tcMetaTy integerTyConName
        ; list_ty <- tcMetaTy listTyConName
        ; checkWiredInTyCon doubleTyCon
        ; let deflt_tys = opt_deflt extended_defaults [unitTy, list_ty]
                          -- Note [Extended defaults]
                          ++ [integer_ty, doubleTy]
                          ++ opt_deflt ovl_strings [stringTy]
        ; return (deflt_tys, flags) } } }
  where
    opt_deflt True  xs = xs
    opt_deflt False _  = []

{-
Note [Extended defaults]
~~~~~~~~~~~~~~~~~~~~~
In interative mode (or with -XExtendedDefaultRules) we add () as the first type we
try when defaulting.  This has very little real impact, except in the following case.
Consider:
        Text.Printf.printf "hello"
This has type (forall a. IO a); it prints "hello", and returns 'undefined'.  We don't
want the GHCi repl loop to try to print that 'undefined'.  The neatest thing is to
default the 'a' to (), rather than to Integer (which is what would otherwise happen;
and then GHCi doesn't attempt to print the ().  So in interactive mode, we add
() to the list of defaulting types.  See Trac #1200.

Additionally, the list type [] is added as a default specialization for
Traversable and Foldable. As such the default default list now has types of
varying kinds, e.g. ([] :: * -> *)  and (Integer :: *).

************************************************************************
*                                                                      *
\subsection{The InstInfo type}
*                                                                      *
************************************************************************

The InstInfo type summarises the information in an instance declaration

    instance c => k (t tvs) where b

It is used just for *local* instance decls (not ones from interface files).
But local instance decls includes
        - derived ones
        - generic ones
as well as explicit user written ones.
-}

data InstInfo a
  = InstInfo
      { iSpec   :: ClsInst          -- Includes the dfun id
      , iBinds  :: InstBindings a
      }

iDFunId :: InstInfo a -> DFunId
iDFunId info = instanceDFunId (iSpec info)

data InstBindings a
  = InstBindings
      { ib_tyvars  :: [Name]   -- Names of the tyvars from the instance head
                               -- that are lexically in scope in the bindings
                               -- Must correspond 1-1 with the forall'd tyvars
                               -- of the dfun Id.  When typechecking, we are
                               -- going to extend the typechecker's envt with
                               --     ib_tyvars -> dfun_forall_tyvars

      , ib_binds   :: LHsBinds a    -- Bindings for the instance methods

      , ib_pragmas :: [LSig a]      -- User pragmas recorded for generating
                                    -- specialised instances

      , ib_extensions :: [LangExt.Extension] -- Any extra extensions that should
                                             -- be enabled when type-checking
                                             -- this instance; needed for
                                             -- GeneralizedNewtypeDeriving

      , ib_derived :: Bool
           -- True <=> This code was generated by GHC from a deriving clause
           --          or standalone deriving declaration
           --          Used only to improve error messages
      }

instance (OutputableBndrId (GhcPass a))
       => Outputable (InstInfo (GhcPass a)) where
    ppr = pprInstInfoDetails

pprInstInfoDetails :: (OutputableBndrId (GhcPass a))
                   => InstInfo (GhcPass a) -> SDoc
pprInstInfoDetails info
   = hang (pprInstanceHdr (iSpec info) <+> text "where")
        2 (details (iBinds info))
  where
    details (InstBindings { ib_binds = b }) = pprLHsBinds b

simpleInstInfoClsTy :: InstInfo a -> (Class, Type)
simpleInstInfoClsTy info = case instanceHead (iSpec info) of
                           (_, cls, [ty]) -> (cls, ty)
                           _ -> panic "simpleInstInfoClsTy"

simpleInstInfoTy :: InstInfo a -> Type
simpleInstInfoTy info = snd (simpleInstInfoClsTy info)

simpleInstInfoTyCon :: InstInfo a -> TyCon
  -- Gets the type constructor for a simple instance declaration,
  -- i.e. one of the form       instance (...) => C (T a b c) where ...
simpleInstInfoTyCon inst = tcTyConAppTyCon (simpleInstInfoTy inst)

-- | Make a name for the dict fun for an instance decl.  It's an *external*
-- name, like other top-level names, and hence must be made with
-- newGlobalBinder.
newDFunName :: Class -> [Type] -> SrcSpan -> TcM Name
newDFunName clas tys loc
  = do  { is_boot <- tcIsHsBootOrSig
        ; mod     <- getModule
        ; let info_string = occNameString (getOccName clas) ++
                            concatMap (occNameString.getDFunTyKey) tys
        ; dfun_occ <- chooseUniqueOccTc (mkDFunOcc info_string is_boot)
        ; newGlobalBinder mod dfun_occ loc }

-- | Special case of 'newDFunName' to generate dict fun name for a single TyCon.
newDFunName' :: Class -> TyCon -> TcM Name
newDFunName' clas tycon        -- Just a simple wrapper
  = do { loc <- getSrcSpanM     -- The location of the instance decl,
                                -- not of the tycon
       ; newDFunName clas [mkTyConApp tycon []] loc }
       -- The type passed to newDFunName is only used to generate
       -- a suitable string; hence the empty type arg list

{-
Make a name for the representation tycon of a family instance.  It's an
*external* name, like other top-level names, and hence must be made with
newGlobalBinder.
-}

newFamInstTyConName :: Located Name -> [Type] -> TcM Name
newFamInstTyConName (L loc name) tys = mk_fam_inst_name id loc name [tys]

newFamInstAxiomName :: Located Name -> [[Type]] -> TcM Name
newFamInstAxiomName (L loc name) branches
  = mk_fam_inst_name mkInstTyCoOcc loc name branches

mk_fam_inst_name :: (OccName -> OccName) -> SrcSpan -> Name -> [[Type]] -> TcM Name
mk_fam_inst_name adaptOcc loc tc_name tyss
  = do  { mod   <- getModule
        ; let info_string = occNameString (getOccName tc_name) ++
                            intercalate "|" ty_strings
        ; occ   <- chooseUniqueOccTc (mkInstTyTcOcc info_string)
        ; newGlobalBinder mod (adaptOcc occ) loc }
  where
    ty_strings = map (concatMap (occNameString . getDFunTyKey)) tyss

{-
Stable names used for foreign exports and annotations.
For stable names, the name must be unique (see #1533).  If the
same thing has several stable Ids based on it, the
top-level bindings generated must not have the same name.
Hence we create an External name (doesn't change), and we
append a Unique to the string right here.
-}

mkStableIdFromString :: String -> Type -> SrcSpan -> (OccName -> OccName) -> TcM TcId
mkStableIdFromString str sig_ty loc occ_wrapper = do
    uniq <- newUnique
    mod <- getModule
    name <- mkWrapperName "stable" str
    let occ = mkVarOccFS name :: OccName
        gnm = mkExternalName uniq mod (occ_wrapper occ) loc :: Name
        id  = mkExportedVanillaId gnm sig_ty :: Id
    return id

mkStableIdFromName :: Name -> Type -> SrcSpan -> (OccName -> OccName) -> TcM TcId
mkStableIdFromName nm = mkStableIdFromString (getOccString nm)

mkWrapperName :: (MonadIO m, HasDynFlags m, HasModule m)
              => String -> String -> m FastString
mkWrapperName what nameBase
    = do dflags <- getDynFlags
         thisMod <- getModule
         let -- Note [Generating fresh names for ccall wrapper]
             wrapperRef = nextWrapperNum dflags
             pkg = unitIdString  (moduleUnitId thisMod)
             mod = moduleNameString (moduleName      thisMod)
         wrapperNum <- liftIO $ atomicModifyIORef' wrapperRef $ \mod_env ->
             let num = lookupWithDefaultModuleEnv mod_env 0 thisMod
                 mod_env' = extendModuleEnv mod_env thisMod (num+1)
             in (mod_env', num)
         let components = [what, show wrapperNum, pkg, mod, nameBase]
         return $ mkFastString $ zEncodeString $ intercalate ":" components

{-
Note [Generating fresh names for FFI wrappers]

We used to use a unique, rather than nextWrapperNum, to distinguish
between FFI wrapper functions. However, the wrapper names that we
generate are external names. This means that if a call to them ends up
in an unfolding, then we can't alpha-rename them, and thus if the
unique randomly changes from one compile to another then we get a
spurious ABI change (#4012).

The wrapper counter has to be per-module, not global, so that the number we end
up using is not dependent on the modules compiled before the current one.
-}

{-
************************************************************************
*                                                                      *
\subsection{Errors}
*                                                                      *
************************************************************************
-}

pprBinders :: [Name] -> SDoc
-- Used in error messages
-- Use quotes for a single one; they look a bit "busy" for several
pprBinders [bndr] = quotes (ppr bndr)
pprBinders bndrs  = pprWithCommas ppr bndrs

notFound :: Name -> TcM TyThing
notFound name
  = do { lcl_env <- getLclEnv
       ; let stage = tcl_th_ctxt lcl_env
       ; case stage of   -- See Note [Out of scope might be a staging error]
           Splice {}
             | isUnboundName name -> failM  -- If the name really isn't in scope
                                            -- don't report it again (Trac #11941)
             | otherwise -> stageRestrictionError (quotes (ppr name))
           _ -> failWithTc $
                vcat[text "GHC internal error:" <+> quotes (ppr name) <+>
                     text "is not in scope during type checking, but it passed the renamer",
                     text "tcl_env of environment:" <+> ppr (tcl_env lcl_env)]
                       -- Take care: printing the whole gbl env can
                       -- cause an infinite loop, in the case where we
                       -- are in the middle of a recursive TyCon/Class group;
                       -- so let's just not print it!  Getting a loop here is
                       -- very unhelpful, because it hides one compiler bug with another
       }

wrongThingErr :: String -> TcTyThing -> Name -> TcM a
-- It's important that this only calls pprTcTyThingCategory, which in
-- turn does not look at the details of the TcTyThing.
-- See Note [Placeholder PatSyn kinds] in TcBinds
wrongThingErr expected thing name
  = failWithTc (pprTcTyThingCategory thing <+> quotes (ppr name) <+>
                text "used as a" <+> text expected)

{- Note [Out of scope might be a staging error]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  x = 3
  data T = MkT $(foo x)

where 'foo' is imported from somewhere.

This is really a staging error, because we can't run code involving 'x'.
But in fact the type checker processes types first, so 'x' won't even be
in the type envt when we look for it in $(foo x).  So inside splices we
report something missing from the type env as a staging error.
See Trac #5752 and #5795.
-}
