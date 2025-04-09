-- (c) The University of Glasgow 2006
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}  -- instance MonadThings is necessarily an
                                       -- orphan
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module GHC.Tc.Utils.Env(
        TyThing(..), TcTyThing(..), TcId,

        -- Instance environment, and InstInfo type
        InstInfo(..), iDFunId, pprInstInfoDetails,
        simpleInstInfoClsTy, simpleInstInfoTy, simpleInstInfoTyCon,
        InstBindings(..),

        -- Global environment
        tcExtendGlobalEnv, tcExtendTyConEnv,
        tcExtendGlobalEnvImplicit, setGlobalTypeEnv,
        tcExtendGlobalValEnv, tcTyThBinders,
        tcLookupLocatedGlobal, tcLookupGlobal, tcLookupGlobalOnly,
        tcLookupTyCon, tcLookupClass,
        tcLookupDataCon, tcLookupPatSyn, tcLookupConLike,
        tcLookupRecSelParent,
        tcLookupLocatedGlobalId, tcLookupLocatedTyCon,
        tcLookupLocatedClass, tcLookupAxiom,
        lookupGlobal, lookupGlobal_maybe,
        addTypecheckedBinds,
        failIllegalTyCon, failIllegalTyVar,

        -- Local environment
        tcExtendKindEnv, tcExtendKindEnvList,
        tcExtendTyVarEnv, tcExtendNameTyVarEnv,
        tcExtendLetEnv, tcExtendSigIds, tcExtendRecIds,
        tcExtendIdEnv, tcExtendIdEnv1, tcExtendIdEnv2,
        tcExtendBinderStack, tcExtendLocalTypeEnv,
        isTypeClosedLetBndr,

        tcLookup, tcLookupLocated, tcLookupLocalIds,
        tcLookupId, tcLookupIdMaybe, tcLookupTyVar,
        tcLookupTcTyCon,
        tcLookupLcl_maybe,
        getInLocalScope,
        wrongThingErr, pprBinders,

        tcAddDataFamConPlaceholders, tcAddPatSynPlaceholders, tcAddKindSigPlaceholders,
        getTypeSigNames,
        tcExtendRecEnv,         -- For knot-tying

        -- Instances
        tcLookupInstance, tcGetInstEnvs,

        -- Rules
        tcExtendRules,

        -- Defaults
        tcGetDefaultTys,

        -- Template Haskell stuff
        StageCheckReason(..),
        checkWellStaged, tcMetaTy, thLevel,
        topIdLvl, isBrackStage,

        -- New Ids
        newDFunName,
        newFamInstTyConName, newFamInstAxiomName,
        mkStableIdFromString, mkStableIdFromName,
        mkWrapperName, tcGetClsDefaults,
  ) where

import GHC.Prelude


import GHC.Driver.Env
import GHC.Driver.Env.KnotVars
import GHC.Driver.DynFlags

import GHC.Builtin.Names
import GHC.Builtin.Types

import GHC.Runtime.Context

import GHC.Hs

import GHC.Iface.Env
import GHC.Iface.Load

import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcType
import {-# SOURCE #-} GHC.Tc.Utils.TcMType ( tcCheckUsage )
import GHC.Tc.Types.LclEnv

import GHC.Core.InstEnv
import GHC.Core.DataCon ( DataCon, dataConTyCon, flSelector )
import GHC.Core.PatSyn  ( PatSyn )
import GHC.Core.ConLike
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Core.Type
import GHC.Core.Coercion.Axiom
import GHC.Core.Class


import GHC.Unit.Module
import GHC.Unit.Module.ModDetails
import GHC.Unit.Home
import GHC.Unit.Home.Graph
import GHC.Unit.Home.ModInfo
import GHC.Unit.External

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Encoding
import GHC.Utils.Misc ( HasDebugCallStack )

import GHC.Data.FastString
import GHC.Data.List.SetOps
import GHC.Data.Maybe( MaybeErr(..), orElse, maybeToList, fromMaybe )

import GHC.Types.SrcLoc
import GHC.Types.Basic hiding( SuccessFlag(..) )
import GHC.Types.TypeEnv
import GHC.Types.SourceFile
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Name.Env
import GHC.Types.DefaultEnv ( DefaultEnv, ClassDefaults(..),
                              defaultEnv, emptyDefaultEnv, lookupDefaultEnv, unitDefaultEnv )
import GHC.Types.Error
import GHC.Types.Id
import GHC.Types.Id.Info ( RecSelParent(..) )
import GHC.Types.Name.Reader
import GHC.Types.TyThing
import GHC.Types.Unique.Set ( nonDetEltsUniqSet )
import qualified GHC.LanguageExtensions as LangExt

import GHC.Iface.Errors.Types
import GHC.Rename.Unbound ( unknownNameSuggestions )
import GHC.Tc.Errors.Types.PromotionErr
import {-# SOURCE #-} GHC.Tc.Errors.Hole (getHoleFitDispConfig)

import Control.Monad
import Data.IORef
import Data.List          ( intercalate )
import qualified Data.List.NonEmpty as NE

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
            Failed err      ->
              let msg = case err of
                          Left name -> text "Could not find local name:" <+> ppr name
                          Right err -> pprDiagnostic err
              in pprPanic "lookupGlobal" msg
        }
lookupGlobal_maybe :: HscEnv -> Name -> IO (MaybeErr (Either Name IfaceMessage) TyThing)
-- This may look up an Id that one has previously looked up.
-- If so, we are going to read its interface file, and add its bindings
-- to the ExternalPackageTable.
lookupGlobal_maybe hsc_env name
  = do  {    -- Try local envt
          let mod = icInteractiveModule (hsc_IC hsc_env)
              mhome_unit = hsc_home_unit_maybe hsc_env
              tcg_semantic_mod = homeModuleInstantiation mhome_unit mod

        ; if nameIsLocalOrFrom tcg_semantic_mod name
          then return $ Failed $ Left name
              -- Internal names can happen in GHCi
          else do
            res <- lookupImported_maybe hsc_env name
            -- Try home package table and external package table
            return $ case res of
              Succeeded ok -> Succeeded ok
              Failed   err -> Failed (Right err)
        }

lookupImported_maybe :: HscEnv -> Name -> IO (MaybeErr IfaceMessage TyThing)
-- Returns (Failed err) if we can't find the interface file for the thing
lookupImported_maybe hsc_env name
  = do  { mb_thing <- lookupType hsc_env name
        ; case mb_thing of
            Just thing -> return (Succeeded thing)
            Nothing    -> importDecl_maybe hsc_env name
        }

importDecl_maybe :: HscEnv -> Name -> IO (MaybeErr IfaceMessage TyThing)
importDecl_maybe hsc_env name
  | Just thing <- wiredInNameTyThing_maybe name
  = do  { when (needWiredInHomeIface thing)
               (initIfaceLoad hsc_env (loadWiredInHomeIface name))
                -- See Note [Loading instances for wired-in things]
        ; return (Succeeded thing) }
  | otherwise
  = initIfaceLoad hsc_env (importDecl name)

addTypecheckedBinds :: TcGblEnv -> [LHsBinds GhcTc] -> TcGblEnv
addTypecheckedBinds tcg_env binds
  | isHsBootOrSig (tcg_src tcg_env) = tcg_env
    -- Do not add the code for record-selector bindings
    -- when compiling hs-boot files
  | otherwise = tcg_env { tcg_binds = foldr (++)
                                            (tcg_binds tcg_env)
                                            binds }
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


tcLookupLocatedGlobal :: LocatedA Name -> TcM TyThing
-- c.f. GHC.IfaceToCore.tcIfaceGlobal
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
            Failed msg      -> failWithTc (TcRnInterfaceError msg)
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
        _                          -> wrongThingErr WrongThingDataCon (AGlobal thing) name

tcLookupPatSyn :: Name -> TcM PatSyn
tcLookupPatSyn name = do
    thing <- tcLookupGlobal name
    case thing of
        AConLike (PatSynCon ps) -> return ps
        _                       -> wrongThingErr WrongThingPatSyn (AGlobal thing) name

tcLookupConLike :: WithUserRdr Name -> TcM ConLike
tcLookupConLike qname@(WithUserRdr _ name) = do
    thing <- tcLookupGlobal name
    case thing of
        AConLike cl -> return cl
        ATyCon  {}  -> failIllegalTyCon WL_ConLike qname
        _           -> wrongThingErr WrongThingConLike (AGlobal thing) name

tcLookupRecSelParent :: HsRecUpdParent GhcRn -> TcM RecSelParent
tcLookupRecSelParent (RnRecUpdParent { rnRecUpdCons = cons })
  = case any_con of
      PatSynName ps ->
        RecSelPatSyn <$> tcLookupPatSyn ps
      DataConName dc ->
        RecSelData . dataConTyCon <$> tcLookupDataCon dc
  where
    any_con = head $ nonDetEltsUniqSet cons
      -- Any constructor will give the same result here.

tcLookupClass :: Name -> TcM Class
tcLookupClass name = do
    thing <- tcLookupGlobal name
    case thing of
        ATyCon tc | Just cls <- tyConClass_maybe tc -> return cls
        _                                           -> wrongThingErr WrongThingClass (AGlobal thing) name

tcLookupTyCon :: Name -> TcM TyCon
tcLookupTyCon name = do
    thing <- tcLookupGlobal name
    case thing of
        ATyCon tc -> return tc
        _         -> wrongThingErr WrongThingTyCon (AGlobal thing) name

tcLookupAxiom :: Name -> TcM (CoAxiom Branched)
tcLookupAxiom name = do
    thing <- tcLookupGlobal name
    case thing of
        ACoAxiom ax -> return ax
        _           -> wrongThingErr WrongThingAxiom (AGlobal thing) name

tcLookupLocatedGlobalId :: LocatedA Name -> TcM Id
tcLookupLocatedGlobalId = addLocM tcLookupId

tcLookupLocatedClass :: LocatedA Name -> TcM Class
tcLookupLocatedClass = addLocM tcLookupClass

tcLookupLocatedTyCon :: LocatedN Name -> TcM TyCon
tcLookupLocatedTyCon = addLocM tcLookupTyCon

-- Find the instance that exactly matches a type class application.  The class arguments must be precisely
-- the same as in the instance declaration (modulo renaming & casts).
--
tcLookupInstance :: Class -> [Type] -> TcM ClsInst
tcLookupInstance cls tys
  = do { instEnv <- tcGetInstEnvs
       ; let inst = lookupUniqueInstEnv instEnv cls tys >>= \ (inst, tys) ->
                    if uniqueTyVars tys then Right inst else Left LookupInstErrNotExact
        ; case inst of
          Right i -> return i
          Left err -> failWithTc (TcRnLookupInstance cls tys err)
       }
  where
    uniqueTyVars tys = all isTyVarTy tys
                    && hasNoDups (map getTyVar tys)

-- | Get the default types for classes
-- explicitly not combined to be use for `reportClashingDefaultImports`
tcGetClsDefaults :: [Module] -> TcM [DefaultEnv]
tcGetClsDefaults mods = do
  hug <- hsc_HUG <$> getTopEnv
  module_env_defaults <- eps_defaults <$> getEps
  liftIO $ mapMaybeM (lookupClsDefault hug module_env_defaults) mods

lookupClsDefault :: HomeUnitGraph -> ModuleEnv DefaultEnv -> Module -> IO (Maybe DefaultEnv)
lookupClsDefault hug module_env_defaults mod =
  lookupHugByModule mod hug >>= \case
             Just hm -> pure $ Just $ md_defaults $ hm_details hm
             Nothing -> pure $ lookupModuleEnv module_env_defaults mod

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

-- Illegal term-level use of type things
failIllegalTyCon :: WhatLooking -> WithUserRdr Name -> TcM a
failIllegalTyVar :: WithUserRdr Name -> TcM a
(failIllegalTyCon, failIllegalTyVar) = (fail_tycon, fail_tyvar)
  where
    fail_tycon what_looking (WithUserRdr rdr tc_nm) = do
      gre <- getGlobalRdrEnv
      let mb_gre = lookupGRE_Name gre tc_nm
          err = case greInfo <$> mb_gre of
            Just (IAmTyCon ClassFlavour) -> ClassTE
            _ -> TyConTE
      fail_with_msg what_looking dataName rdr tc_nm (TermLevelUseGRE <$> mb_gre) err

    fail_tyvar (WithUserRdr rdr nm) =
      fail_with_msg WL_Term varName rdr nm (Just TermLevelUseTyVar) TyVarTE

    fail_with_msg what_looking whatName rdr nm pprov err = do
      required_type_arguments <- xoptM LangExt.RequiredTypeArguments
      (imp_errs, hints) <- get_suggestions required_type_arguments what_looking whatName rdr
      hfdc <- getHoleFitDispConfig
      unit_state <- hsc_units <$> getTopEnv
      let
        want_simple = want_simple_msg hints
        msg = TcRnIllegalTermLevelUse want_simple rdr nm err
        info = ErrInfo { errInfoContext =
                           if want_simple
                           then []
                           else maybeToList $ fmap (TermLevelUseCtxt nm) pprov
                       , errInfoSupplementary =
                           fmap ((hfdc,) . (:[]) . SupplementaryImportErrors) $
                             NE.nonEmpty imp_errs
                       , errInfoHints = hints
                       }
      failWithTc $ TcRnMessageWithInfo unit_state (mkDetailedMessage info msg)

    get_suggestions required_type_arguments what_looking ns rdr = do
      show_helpful_errors <- goptM Opt_HelpfulErrors
      if not show_helpful_errors || (required_type_arguments && isVarNameSpace ns)
      then return ([], [])  -- See Note [Suppress hints with RequiredTypeArguments]
      else do
        let rdr' = fromMaybe rdr (demoteRdrName rdr)
        lcl_env <- getLocalRdrEnv
        unknownNameSuggestions lcl_env what_looking rdr'

-- | Should we display a simpler "out of scope" message to the user, instead of
-- a full-blown "illegal term-level use" message?
--
-- See Note [Simpler "illegal term-level use" errors]
want_simple_msg :: [GhcHint] -> Bool
want_simple_msg hints = any relevant_suggestion hints
  where
    relevant_suggestion = \case
      ImportSuggestion {} -> True
      SuggestSimilarNames {} -> True
      _ -> False

{- Note [Simpler "illegal term-level use" errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we rename the occurrence of A in the definition of 'f' in the following program:

  module M1 where
    data A = A Int
  module M2 where
    import M1 (A)
    f x = A x

we initially resolve 'A' to the type constructor 'A', in order to support
-XRequiredTypeArguments. Then we come to find out that a type is illegal in
this position. It is more user-friendly to report the problem as:

  Data constructor out of scope: A

rather than:

  Illegal term-level use of type constructor A

This was reported in #23982.

To achieve this, in 'failIllegalTyCon' and 'failIllegalTyVar', we include a
little heuristic to decide whether to emit an "out of scope" message rather than
an "illegal term-level use" message: when we have a term to suggest to the user,
then give the simpler "out of scope" error message.

For instance, in the example above, we suggest extending the import list to

  import M1 (A(A))

to bring the data constructor A into scope. We thus emit the following message:

    Data constructor out of scope: A
    Suggested fix:
      Add ‘A’ to the import list in the import of M1

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
         ; case lookupKnotVars (tcg_type_env_var tcg_env) (tcg_mod tcg_env) of
              Just tcg_env_var -> writeMutVar tcg_env_var new_type_env
              Nothing -> return ()
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

-- Given a [TyThing] of "non-value" bindings coming from type decls
-- (constructors, field selectors, class methods) return their
-- TH binding levels (to be added to a LclEnv).
-- See GHC ticket #17820 .
tcTyThBinders :: [TyThing] -> TcM ThBindEnv
tcTyThBinders implicit_things = do
  stage <- getStage
  let th_lvl  = thLevel stage
      th_bndrs = mkNameEnv
                  [ ( n , (TopLevel, th_lvl) ) | n <- names ]
  return th_bndrs
  where
    names = concatMap get_names implicit_things
    get_names (AConLike acl) =
      conLikeName acl : map flSelector (conLikeFieldLabels acl)
    get_names (AnId i) = [idName i]
    get_names _ = []

tcExtendGlobalValEnv :: [Id] -> TcM a -> TcM a
  -- Same deal as tcExtendGlobalEnv, but for Ids
tcExtendGlobalValEnv ids thing_inside
  = tcExtendGlobalEnvImplicit [AnId id | id <- ids] thing_inside

tcExtendRecEnv :: [(Name,TyThing)] -> TcM r -> TcM r
-- Extend the global environments for the type/class knot tying game
-- Just like tcExtendGlobalEnv, except the argument is a list of pairs
tcExtendRecEnv gbl_stuff thing_inside
 = do  { tcg_env <- getGblEnv
       ; let ge'      = extendNameEnvList (tcg_type_env tcg_env) gbl_stuff
             tcg_env' = tcg_env { tcg_type_env = ge' }
         -- No need for setGlobalTypeEnv (which side-effects the
         -- tcg_type_env_var); tcExtendRecEnv is used just
         -- when kind-check a group of type/class decls. It would
         -- in any case be wrong for an interface-file decl to end up
         -- with a TcTyCon in it!
       ; setGblEnv tcg_env' thing_inside }

{-
************************************************************************
*                                                                      *
\subsection{The local environment}
*                                                                      *
************************************************************************
-}

tcLookupLocated :: LocatedA Name -> TcM TcTyThing
tcLookupLocated = addLocM tcLookup

tcLookupLcl_maybe :: Name -> TcM (Maybe TcTyThing)
tcLookupLcl_maybe name
  = do { local_env <- getLclTypeEnv
       ; return (lookupNameEnv local_env name) }

tcLookup :: Name -> TcM TcTyThing
tcLookup name = do
    local_env <- getLclTypeEnv
    case lookupNameEnv local_env name of
        Just thing -> return thing
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
-- The Id is never a DataCon. (Why does that matter? see GHC.Tc.Gen.Expr.tcId)
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
       ; return (map (lookup (getLclEnvTypeEnv env)) ns) }
  where
    lookup lenv name
        = case lookupNameEnv lenv name of
                Just (ATcId { tct_id = id }) ->  id
                _ -> pprPanic "tcLookupLocalIds" (ppr name)

-- inferInitialKind has made a suitably-shaped kind for the type or class
-- Look it up in the local environment. This is used only for tycons
-- that we're currently type-checking, so we're sure to find a TcTyCon.
tcLookupTcTyCon :: HasDebugCallStack => Name -> TcM TcTyCon
tcLookupTcTyCon name = do
    thing <- tcLookup name
    case thing of
        ATcTyCon tc -> return tc
        _           -> pprPanic "tcLookupTcTyCon" (ppr name)


getInLocalScope :: TcM (Name -> Bool)
getInLocalScope = do { lcl_env <- getLclTypeEnv
                     ; return (`elemNameEnv` lcl_env) }

tcExtendKindEnvList :: [(Name, TcTyThing)] -> TcM r -> TcM r
-- Used only during kind checking, for TcThings that are
--      ATcTyCon or APromotionErr
-- No need to update the global tyvars, or tcl_th_bndrs, or tcl_rdr
tcExtendKindEnvList things thing_inside
  = do { traceTc "tcExtendKindEnvList" (ppr things)
       ; updLclCtxt upd_env thing_inside }
  where
    upd_env env = env { tcl_env = extendNameEnvList (tcl_env env) things }

tcExtendKindEnv :: NameEnv TcTyThing -> TcM r -> TcM r
-- A variant of tcExtendKindEvnList
tcExtendKindEnv extra_env thing_inside
  = do { traceTc "tcExtendKindEnv" (ppr extra_env)
       ; updLclCtxt upd_env thing_inside }
  where
    upd_env env = env { tcl_env = tcl_env env `plusNameEnv` extra_env }

-----------------------
-- Scoped type and kind variables
tcExtendTyVarEnv :: [TyVar] -> TcM r -> TcM r
tcExtendTyVarEnv tvs thing_inside
  -- MP: This silently coerces TyVar to TcTyVar.
  = tcExtendNameTyVarEnv (mkTyVarNamePairs tvs) thing_inside

tcExtendNameTyVarEnv :: [(Name,TcTyVar)] -> TcM r -> TcM r
tcExtendNameTyVarEnv binds thing_inside
  -- this should be used only for explicitly mentioned scoped variables.
  -- thus, no coercion variables
  = tc_extend_local_env NotTopLevel names $
        tcExtendBinderStack tv_binds $
        thing_inside
  where
    tv_binds :: [TcBinder]
    tv_binds = [TcTvBndr name tv | (name,tv) <- binds]

    names = [(name, ATyVar name tv) | (name, tv) <- binds]

isTypeClosedLetBndr :: Id -> Bool
-- See Note [Bindings with closed types: ClosedTypeId] in GHC.Tc.Types
isTypeClosedLetBndr = noFreeVarsOfType . idType

tcExtendRecIds :: [(Name, TcId)] -> TcM a -> TcM a
-- Used for binding the recursive uses of Ids in a binding
-- both top-level value bindings and nested let/where-bindings
-- Does not extend the TcBinderStack
tcExtendRecIds pairs thing_inside
  = tc_extend_local_env NotTopLevel
          [ (name, ATcId { tct_id   = let_id
                         , tct_info = NonClosedLet emptyNameSet False })
          | (name, let_id) <- pairs ] $
    thing_inside

tcExtendSigIds :: TopLevelFlag -> [TcId] -> TcM a -> TcM a
-- Used for binding the Ids that have a complete user type signature
-- Does not extend the TcBinderStack
tcExtendSigIds top_lvl sig_ids thing_inside
  = tc_extend_local_env top_lvl
          [ (idName id, ATcId { tct_id   = id
                              , tct_info = info })
          | id <- sig_ids
          , let closed = isTypeClosedLetBndr id
                info   = NonClosedLet emptyNameSet closed ]
     thing_inside


tcExtendLetEnv :: TopLevelFlag -> TcSigFun -> IsGroupClosed
                  -> [Scaled TcId] -> TcM a -> TcM a
-- Used for both top-level value bindings and nested let/where-bindings
-- Adds to the TcBinderStack too
tcExtendLetEnv top_lvl sig_fn (IsGroupClosed fvs fv_type_closed)
               ids thing_inside
  = tcExtendBinderStack [TcIdBndr id top_lvl | Scaled _ id <- ids] $
    tc_extend_local_env top_lvl
          [ (idName id, ATcId { tct_id   = id
                              , tct_info = mk_tct_info id })
          | Scaled _ id <- ids ] $
    foldr check_usage thing_inside scaled_names
  where
    mk_tct_info id
      | type_closed && isEmptyNameSet rhs_fvs = ClosedLet
      | otherwise                             = NonClosedLet rhs_fvs type_closed
      where
        name        = idName id
        rhs_fvs     = lookupNameEnv fvs name `orElse` emptyNameSet
        type_closed = isTypeClosedLetBndr id &&
                      (fv_type_closed || hasCompleteSig sig_fn name)
    scaled_names = [Scaled p (idName id) | Scaled p id <- ids ]
    check_usage :: Scaled Name -> TcM a -> TcM a
    check_usage (Scaled p id) thing_inside = do
      tcCheckUsage id p thing_inside

tcExtendIdEnv :: [TcId] -> TcM a -> TcM a
-- For lambda-bound and case-bound Ids
-- Extends the TcBinderStack as well
tcExtendIdEnv ids thing_inside
  = tcExtendIdEnv2 [(idName id, id) | id <- ids] thing_inside

tcExtendIdEnv1 :: Name -> TcId -> TcM a -> TcM a
-- Exactly like tcExtendIdEnv2, but for a single (name,id) pair
tcExtendIdEnv1 name id thing_inside
  = tcExtendIdEnv2 [(name,id)] thing_inside

tcExtendIdEnv2 :: [(Name,TcId)] -> TcM a -> TcM a
tcExtendIdEnv2 names_w_ids thing_inside
  = tcExtendBinderStack [ TcIdBndr mono_id NotTopLevel
                        | (_,mono_id) <- names_w_ids ] $
    tc_extend_local_env NotTopLevel
            [ (name, ATcId { tct_id = id
                           , tct_info    = NotLetBound })
            | (name,id) <- names_w_ids]
    thing_inside

tc_extend_local_env :: TopLevelFlag -> [(Name, TcTyThing)] -> TcM a -> TcM a
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
        ; updLclCtxt upd_lcl_env thing_inside }
  where
    upd_lcl_env env0@(TcLclCtxt { tcl_th_ctxt  = stage
                               , tcl_rdr      = rdr_env
                               , tcl_th_bndrs = th_bndrs
                               , tcl_env      = lcl_type_env })
       = env0 { tcl_rdr = extendLocalRdrEnvList rdr_env
                          [ n | (n, _) <- extra_env, isInternalName n ]
                          -- The LocalRdrEnv contains only non-top-level names
                          -- (GlobalRdrEnv handles the top level)

              , tcl_th_bndrs = extendNameEnvList th_bndrs
                               [(n, thlvl) | (n, _) <- extra_env]

              , tcl_env = extendNameEnvList lcl_type_env extra_env }
              -- tcl_rdr and tcl_th_bndrs: extend the local LocalRdrEnv and
              -- Template Haskell staging env simultaneously. Reason for extending
              -- LocalRdrEnv: after running a TH splice we need to do renaming.
      where
        thlvl = (top_lvl, thLevel stage)


tcExtendLocalTypeEnv :: [(Name, TcTyThing)] -> TcLclCtxt -> TcLclCtxt
tcExtendLocalTypeEnv tc_ty_things lcl_env@(TcLclCtxt { tcl_env = lcl_type_env })
  = lcl_env { tcl_env = extendNameEnvList lcl_type_env tc_ty_things }

{- *********************************************************************
*                                                                      *
             The TcBinderStack
*                                                                      *
********************************************************************* -}

tcExtendBinderStack :: [TcBinder] -> TcM a -> TcM a
tcExtendBinderStack bndrs thing_inside
  = do { traceTc "tcExtendBinderStack" (ppr bndrs)
       ; updLclCtxt (\env -> env { tcl_bndrs = bndrs ++ tcl_bndrs env })
                   thing_inside }

{- *********************************************************************
*                                                                      *
             Adding placeholders
*                                                                      *
********************************************************************* -}

tcAddDataFamConPlaceholders :: [LInstDecl GhcRn] -> TcM a -> TcM a
-- See Note [AFamDataCon: not promoting data family constructors]
tcAddDataFamConPlaceholders inst_decls thing_inside
  = tcExtendKindEnvList [ (con, APromotionErr FamDataConPE)
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

    get_fi_cons :: DataFamInstDecl GhcRn -> [Name]
    get_fi_cons (DataFamInstDecl { dfid_eqn =
                  FamEqn { feqn_rhs = HsDataDefn { dd_cons = cons } }})
      = map unLoc $ concatMap (getConNames . unLoc) cons


tcAddPatSynPlaceholders :: [PatSynBind GhcRn GhcRn] -> TcM a -> TcM a
-- See Note [Don't promote pattern synonyms]
tcAddPatSynPlaceholders pat_syns thing_inside
  = tcExtendKindEnvList [ (name, APromotionErr PatSynPE)
                        | PSB{ psb_id = L _ name } <- pat_syns ]
       thing_inside

tcAddKindSigPlaceholders :: LHsKind GhcRn -> TcM a -> TcM a
tcAddKindSigPlaceholders kind_sig thing_inside
  = tcExtendKindEnvList [ (name, APromotionErr TypeVariablePE)
                        | name <- hsScopedKvs kind_sig ]
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

#12088 describes this limitation. Of course, when MkT and S live in
different modules then all is well.

Note [Don't promote pattern synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We never promote pattern synonyms.

Consider this (#11265):
  pattern A = True
  instance Eq A
We want a civilised error message from the occurrence of 'A'
in the instance, yet 'A' really has not yet been type checked.

Similarly (#9161)
  {-# LANGUAGE PatternSynonyms, DataKinds #-}
  pattern A = ()
  b :: A
  b = undefined
Here, the type signature for b mentions A.  But A is a pattern
synonym, which is typechecked as part of a group of bindings (for very
good reasons; a view pattern in the RHS may mention a value binding).
It is entirely reasonable to reject this, but to do so we need A to be
in the kind environment when kind-checking the signature for B.

Hence tcAddPatSynPlaceholders adds a binding
    A -> APromotionErr PatSynPE
to the environment. Then GHC.Tc.Gen.HsType.tcTyVar will find A in the kind
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

checkWellStaged :: StageCheckReason -- What the stage check is for
                -> ThLevel      -- Binding level (increases inside brackets)
                -> ThLevel      -- Use stage
                -> TcM ()       -- Fail if badly staged, adding an error
checkWellStaged pp_thing bind_lvl use_lvl
  | use_lvl >= bind_lvl         -- OK! Used later than bound
  = return ()                   -- E.g.  \x -> [| $(f x) |]

  | bind_lvl == outerLevel      -- GHC restriction on top level splices
  = failWithTc (TcRnStageRestriction pp_thing)

  | otherwise                   -- Badly staged
  = failWithTc $                -- E.g.  \x -> $(f x)
    TcRnBadlyStaged pp_thing bind_lvl use_lvl

topIdLvl :: Id -> ThLevel
-- Globals may either be imported, or may be from an earlier "chunk"
-- (separated by declaration splices) of this module.  The former
--  *can* be used inside a top-level splice, but the latter cannot.
-- Hence we give the former impLevel, but the latter topLevel
-- E.g. this is bad:
--      x = [| foo |]
--      $( f x )
-- By the time we are processing the $(f x), the binding for "x"
-- will be in the global env, not the local one.
topIdLvl id | isLocalId id = outerLevel
            | otherwise    = impLevel

tcMetaTy :: Name -> TcM Type
-- Given the name of a Template Haskell data type,
-- return the type
-- E.g. given the name "Expr" return the type "Expr"
tcMetaTy tc_name = do
    t <- tcLookupTyCon tc_name
    return (mkTyConTy t)

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

{- Note [Default class defaults]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In absence of user-defined `default` declarations, the set of class defaults in
effect (i.e. `DefaultEnv`) is determined by the absence or
presence of the `ExtendedDefaultRules` and `OverloadedStrings` extensions. In their
absence, the only rule in effect is `default Num (Integer, Double)` as specified by
Haskell Language Report.

In GHC's internal packages `DefaultEnv` is empty to minimize cross-module dependencies:
the `Num` class or `Integer` type may not even be available in low-level modules. If
you don't do this, attempted defaulting in package ghc-prim causes an actual crash
(attempting to look up the `Integer` type).

A user-defined `default` declaration overrides the defaults for the specified class,
and only for that class.
-}

tcGetDefaultTys :: TcM (DefaultEnv,  -- Default classes and types
                        Bool)        -- True <=> Use extended defaulting rules
tcGetDefaultTys
  = do  { dflags <- getDynFlags
        ; let ovl_strings = xopt LangExt.OverloadedStrings dflags
              extended_defaults = xopt LangExt.ExtendedDefaultRules dflags
                                        -- See also #1974
              builtinDefaults cls tys = ClassDefaults{ cd_class = cls
                                                     , cd_types = tys
                                                     , cd_module = Nothing
                                                     , cd_warn = Nothing }

        -- see Note [Named default declarations] in GHC.Tc.Gen.Default
        ; defaults <- getDeclaredDefaultTys -- User-supplied defaults
        ; this_module <- tcg_mod <$> getGblEnv
        ; let this_unit = moduleUnit this_module
        ; if this_unit == ghcInternalUnit
             -- see Note [Default class defaults]
          then return (defaults, extended_defaults)
          else do
              -- not one of the built-in units
              -- @default Num (Integer, Double)@, plus extensions
              { extDef <- if extended_defaults
                          then do { list_ty <- tcMetaTy listTyConName
                                  ; integer_ty <- tcMetaTy integerTyConName
                                  ; foldableClass <- tcLookupClass foldableClassName
                                  ; showClass <- tcLookupClass showClassName
                                  ; eqClass <- tcLookupClass eqClassName
                                  ; pure $ defaultEnv
                                    [ builtinDefaults foldableClass [list_ty]
                                    , builtinDefaults showClass [unitTy, integer_ty, doubleTy]
                                    , builtinDefaults eqClass [unitTy, integer_ty, doubleTy]
                                    ]
                                  }
                                  -- Note [Extended defaults]
                          else pure emptyDefaultEnv
              ; ovlStr <- if ovl_strings
                          then do { isStringClass <- tcLookupClass isStringClassName
                                  ; pure $ unitDefaultEnv $ builtinDefaults isStringClass [stringTy]
                                  }
                          else pure emptyDefaultEnv
              ; checkWiredInTyCon doubleTyCon
              ; numDef <- case lookupDefaultEnv defaults numClassName of
                   Nothing -> do { integer_ty <- tcMetaTy integerTyConName
                                 ; numClass <- tcLookupClass numClassName
                                 ; pure $ unitDefaultEnv $ builtinDefaults numClass [integer_ty, doubleTy]
                                 }
                   -- The Num class is already user-defaulted, no need to construct the builtin default
                   _ -> pure emptyDefaultEnv
              ; let deflt_tys = mconcat [ extDef, numDef, ovlStr, defaults ]
              ; return (deflt_tys, extended_defaults) } }

{-
Note [Extended defaults]
~~~~~~~~~~~~~~~~~~~~~~~~
In interactive mode (or with -XExtendedDefaultRules) we add () as the first type we
try when defaulting.  This has very little real impact, except in the following case.
Consider:
        Text.Printf.printf "hello"
This has type (forall a. IO a); it prints "hello", and returns 'undefined'.  We don't
want the GHCi repl loop to try to print that 'undefined'.  The neatest thing is to
default the 'a' to (), rather than to Integer (which is what would otherwise happen;
and then GHCi doesn't attempt to print the ().  So in interactive mode, we add
() to the list of defaulting types.  See #1200.

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

instance (OutputableBndrId a)
       => Outputable (InstInfo (GhcPass a)) where
    ppr = pprInstInfoDetails

pprInstInfoDetails :: (OutputableBndrId a)
                   => InstInfo (GhcPass a) -> SDoc
pprInstInfoDetails info
   = hang (pprInstanceHdr (iSpec info) <+> text "where")
        2 (details (iBinds info))
  where
    details (InstBindings { ib_pragmas = p, ib_binds = b }) =
      pprDeclList (pprLHsBindsForUser b p)

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
                            concatMap (occNameString . getDFunTyKey) tys
        ; dfun_occ <- chooseUniqueOccTc (mkDFunOcc info_string is_boot)
        ; newGlobalBinder mod dfun_occ loc }

newFamInstTyConName :: LocatedN Name -> [Type] -> TcM Name
newFamInstTyConName (L loc name) tys = mk_fam_inst_name id (locA loc) name [tys]

newFamInstAxiomName :: LocatedN Name -> [[Type]] -> TcM Name
newFamInstAxiomName (L loc name) branches
  = mk_fam_inst_name mkInstTyCoOcc (locA loc) name branches

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
    nextWrapperNum <- tcg_next_wrapper_num <$> getGblEnv
    name <- mkWrapperName nextWrapperNum "stable" str
    let occ = mkVarOccFS name :: OccName
        gnm = mkExternalName uniq mod (occ_wrapper occ) loc :: Name
        id  = mkExportedVanillaId gnm sig_ty :: Id
    return id

mkStableIdFromName :: Name -> Type -> SrcSpan -> (OccName -> OccName) -> TcM TcId
mkStableIdFromName nm = mkStableIdFromString (getOccString nm)

mkWrapperName :: (MonadIO m, HasModule m)
              => IORef (ModuleEnv Int) -> String -> String -> m FastString
-- ^ @mkWrapperName ref what nameBase@
--
-- See Note [Generating fresh names for FFI wrappers] for @ref@'s purpose.
mkWrapperName wrapperRef what nameBase
    = do thisMod <- getModule
         let pkg = unitString  (moduleUnit thisMod)
             mod = moduleNameString (moduleName      thisMod)
         wrapperNum <- liftIO $ atomicModifyIORef' wrapperRef $ \mod_env ->
             let num = lookupWithDefaultModuleEnv mod_env 0 thisMod
                 mod_env' = extendModuleEnv mod_env thisMod (num+1)
             in (mod_env', num)
         let components = [what, show wrapperNum, pkg, mod, nameBase]
         return $ mkFastString $ zEncodeString $ intercalate ":" components

{-
Note [Generating fresh names for FFI wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
       ; let stage = getLclEnvThStage lcl_env
       ; case stage of   -- See Note [Out of scope might be a staging error]
           Splice {}
             | isUnboundName name -> failM  -- If the name really isn't in scope
                                            -- don't report it again (#11941)
             | otherwise -> failWithTc (TcRnStageRestriction (StageCheckSplice name))

           _ | isTermVarOrFieldNameSpace (nameNameSpace name) ->
               -- This code path is only reachable with RequiredTypeArguments enabled
               -- via the following chain of calls:
               --   `notFound`       called from
               --   `tcLookupGlobal` called from
               --   `tcLookup`       called from
               --   `tcTyVar`
               -- It means the user tried to use a term variable at the type level, e.g.
               --   let { a = 42; f :: a -> a; ... } in ...
               -- If you are seeing this error for any other reason, it is a bug in GHC.
               -- See Note [Demotion of unqualified variables] (W1) in GHC.Rename.Env
               failWithTc $ TcRnUnpromotableThing name TermVariablePE

             | otherwise -> failWithTc $
                TcRnNotInScope (NotInScopeTc (getLclEnvTypeEnv lcl_env)) (getRdrName name)
                  -- Take care: printing the whole gbl env can
                  -- cause an infinite loop, in the case where we
                  -- are in the middle of a recursive TyCon/Class group;
                  -- so let's just not print it!  Getting a loop here is
                  -- very unhelpful, because it hides one compiler bug with another
       }

wrongThingErr :: WrongThingSort -> TcTyThing -> Name -> TcM a
wrongThingErr expected thing name =
  failWithTc (TcRnTyThingUsedWrong expected thing name)

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
See #5752 and #5795.
-}
