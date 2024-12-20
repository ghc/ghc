{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ViewPatterns #-}

-- | Loading interface files
module GHC.Iface.Load (
        -- Importing one thing
        tcLookupImported_maybe, importDecl,
        checkWiredInTyCon, ifCheckWiredInThing,

        -- RnM/TcM functions
        loadModuleInterface, loadModuleInterfaces,
        loadSrcInterface, loadSrcInterface_maybe,
        loadInterfaceForName, loadInterfaceForModule,

        -- IfM functions
        loadInterface,
        loadSysInterface, loadUserInterface, loadPluginInterface,
        loadExternalGraphBelow,
        findAndReadIface, readIface, writeIface,
        flagsToIfCompression,
        moduleFreeHolesPrecise,
        needWiredInHomeIface, loadWiredInHomeIface,

        WhereFrom(..),

        pprModIfaceSimple,
        ifaceStats, pprModIface, showIface,

        getGhcPrimIface,

        module Iface_Errors -- avoids boot files in Ppr modules
   ) where

import GHC.Prelude

import GHC.Platform.Profile

import {-# SOURCE #-} GHC.IfaceToCore
   ( tcIfaceDecls, tcIfaceRules, tcIfaceInst, tcIfaceFamInst
   , tcIfaceAnnotations, tcIfaceCompleteMatches )

import GHC.Driver.Config.Finder
import GHC.Driver.Env
import GHC.Driver.Errors.Types
import GHC.Driver.DynFlags
import GHC.Driver.Hooks
import GHC.Driver.Plugins

import GHC.Iface.Warnings
import GHC.Iface.Syntax
import GHC.Iface.Ext.Fields
import GHC.Iface.Binary
import GHC.Iface.Rename
import GHC.Iface.Env
import GHC.Iface.Errors as Iface_Errors

import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad

import GHC.Utils.Binary   ( BinData(..) )
import GHC.Utils.Error
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import GHC.Utils.Constants (debugIsOn)
import GHC.Utils.Logger
import GHC.Utils.Fingerprint( Fingerprint )

import GHC.Settings.Constants

import GHC.Builtin.Names
import GHC.Builtin.Utils

import GHC.Core.Rules
import GHC.Core.TyCon
import GHC.Core.InstEnv
import GHC.Core.FamInstEnv

import GHC.Types.Annotations
import GHC.Types.Name
import GHC.Types.Name.Cache
import GHC.Types.Name.Env
import GHC.Types.Avail
import GHC.Types.Fixity
import GHC.Types.Fixity.Env
import GHC.Types.SourceError
import GHC.Types.SourceFile
import GHC.Types.SafeHaskell
import GHC.Types.TypeEnv
import GHC.Types.Unique.DSet
import GHC.Types.SrcLoc
import GHC.Types.TyThing
import GHC.Types.PkgQual

import GHC.Unit.External
import GHC.Unit.Module
import GHC.Unit.Module.Warnings
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.Deps
import GHC.Unit.State
import GHC.Unit.Home
import GHC.Unit.Home.PackageTable
import GHC.Unit.Finder
import GHC.Unit.Env
import GHC.Unit.Module.External.Graph

import GHC.Data.Maybe

import Control.Monad
import Data.Map ( toList )
import System.FilePath
import System.Directory
import GHC.Driver.Env.KnotVars
import {-# source #-} GHC.Driver.Main (loadIfaceByteCode)
import GHC.Iface.Errors.Types
import Data.Function ((&))
import qualified Data.Set as Set
import GHC.Unit.Module.Graph
import qualified GHC.Unit.Home.Graph as HUG

{-
************************************************************************
*                                                                      *
*      tcImportDecl is the key function for "faulting in"              *
*      imported things
*                                                                      *
************************************************************************

The main idea is this.  We are chugging along type-checking source code, and
find a reference to GHC.Base.map.  We call tcLookupGlobal, which doesn't find
it in the EPS type envt.  So it
        1 loads GHC.Base.hi
        2 gets the decl for GHC.Base.map
        3 typechecks it via tcIfaceDecl
        4 and adds it to the type env in the EPS

Note that DURING STEP 4, we may find that map's type mentions a type
constructor that also

Notice that for imported things we read the current version from the EPS
mutable variable.  This is important in situations like
        ...$(e1)...$(e2)...
where the code that e1 expands to might import some defns that
also turn out to be needed by the code that e2 expands to.
-}

tcLookupImported_maybe :: Name -> TcM (MaybeErr IfaceMessage TyThing)
-- Returns (Failed err) if we can't find the interface file for the thing
tcLookupImported_maybe name
  = do  { hsc_env <- getTopEnv
        ; mb_thing <- liftIO (lookupType hsc_env name)
        ; case mb_thing of
            Just thing -> return (Succeeded thing)
            Nothing    -> tcImportDecl_maybe name }

tcImportDecl_maybe :: Name -> TcM (MaybeErr IfaceMessage TyThing)
-- Entry point for *source-code* uses of importDecl
tcImportDecl_maybe name
  | Just thing <- wiredInNameTyThing_maybe name
  = do  { when (needWiredInHomeIface thing)
               (initIfaceTcRn (loadWiredInHomeIface name))
                -- See Note [Loading instances for wired-in things]
        ; return (Succeeded thing) }
  | otherwise
  = initIfaceTcRn (importDecl name)

importDecl :: Name -> IfM lcl (MaybeErr IfaceMessage TyThing)
-- Get the TyThing for this Name from an interface file
-- It's not a wired-in thing -- the caller caught that
importDecl name
  = assert (not (isWiredInName name)) $
    do  { logger <- getLogger
        ; liftIO $ trace_if logger nd_doc

        -- Load the interface, which should populate the PTE
        ; mb_iface <- assertPpr (isExternalName name) (ppr name) $
                      loadInterface nd_doc (nameModule name) ImportBySystem
        ; case mb_iface of
          { Failed err_msg -> return $ Failed $
                              Can'tFindInterface err_msg (LookingForName name)
          ; Succeeded _ -> do

        -- Now look it up again; this time we should find it
        { eps <- getEps
        ; case lookupTypeEnv (eps_PTE eps) name of
            Just thing -> return $ Succeeded thing
            Nothing    -> return $ Failed $
              Can'tFindNameInInterface name
              (filter is_interesting $ nonDetNameEnvElts $ eps_PTE eps)
    }}}
  where
    nd_doc = text "Need decl for" <+> ppr name
    is_interesting thing = nameModule name == nameModule (getName thing)


{-
************************************************************************
*                                                                      *
           Checks for wired-in things
*                                                                      *
************************************************************************

Note [Loading instances for wired-in things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to make sure that we have at least *read* the interface files
for any module with an instance decl or RULE that we might want.

* If the instance decl is an orphan, we have a whole separate mechanism
  (loadOrphanModules)

* If the instance decl is not an orphan, then the act of looking at the
  TyCon or Class will force in the defining module for the
  TyCon/Class, and hence the instance decl

* BUT, if the TyCon is a wired-in TyCon, we don't really need its interface;
  but we must make sure we read its interface in case it has instances or
  rules.  That is what GHC.Iface.Load.loadWiredInHomeIface does.  It's called
  from GHC.IfaceToCore.{tcImportDecl, checkWiredInTyCon, ifCheckWiredInThing}

* HOWEVER, only do this for TyCons.  There are no wired-in Classes.  There
  are some wired-in Ids, but we don't want to load their interfaces. For
  example, Control.Exception.Base.recSelError is wired in, but that module
  is compiled late in the base library, and we don't want to force it to
  load before it's been compiled!

All of this is done by the type checker. The renamer plays no role.
(It used to, but no longer.)
-}

checkWiredInTyCon :: TyCon -> TcM ()
-- Ensure that the home module of the TyCon (and hence its instances)
-- are loaded. See Note [Loading instances for wired-in things]
-- It might not be a wired-in tycon (see the calls in GHC.Tc.Utils.Unify),
-- in which case this is a no-op.
checkWiredInTyCon tc
  | not (isWiredInName tc_name)
  = return ()
  | otherwise
  = do  { mod <- getModule
        ; logger <- getLogger
        ; liftIO $ trace_if logger (text "checkWiredInTyCon" <+> ppr tc_name $$ ppr mod)
        ; assert (isExternalName tc_name )
          when (mod /= nameModule tc_name)
               (initIfaceTcRn (loadWiredInHomeIface tc_name))
                -- Don't look for (non-existent) Float.hi when
                -- compiling Float.hs, which mentions Float of course
                -- A bit yukky to call initIfaceTcRn here
        }
  where
    tc_name = tyConName tc

ifCheckWiredInThing :: TyThing -> IfL ()
-- Even though we are in an interface file, we want to make
-- sure the instances of a wired-in thing are loaded (imagine f :: Double -> Double)
-- Ditto want to ensure that RULES are loaded too
-- See Note [Loading instances for wired-in things]
ifCheckWiredInThing thing
  = do  { mod <- getIfModule
                -- Check whether we are typechecking the interface for this
                -- very module.  E.g when compiling the base library in --make mode
                -- we may typecheck GHC.Base.hi. At that point, GHC.Base is not in
                -- the HPT, so without the test we'll demand-load it into the PIT!
                -- C.f. the same test in checkWiredInTyCon above
        ; let name = getName thing
        ; assertPpr (isExternalName name) (ppr name) $
          when (needWiredInHomeIface thing && mod /= nameModule name)
               (loadWiredInHomeIface name) }

needWiredInHomeIface :: TyThing -> Bool
-- Only for TyCons; see Note [Loading instances for wired-in things]
needWiredInHomeIface (ATyCon {}) = True
needWiredInHomeIface _           = False


{-
************************************************************************
*                                                                      *
        loadSrcInterface, loadOrphanModules, loadInterfaceForName

                These three are called from TcM-land
*                                                                      *
************************************************************************
-}

-- | Load the interface corresponding to an @import@ directive in
-- source code.  On a failure, fail in the monad with an error message.
loadSrcInterface :: SDoc
                 -> ModuleName
                 -> IsBootInterface     -- {-# SOURCE #-} ?
                 -> PkgQual             -- "package", if any
                 -> RnM ModIface

loadSrcInterface doc mod want_boot maybe_pkg
  = do { res <- loadSrcInterface_maybe doc mod want_boot maybe_pkg
       ; case res of
           Failed    err ->
             failWithTc $
               TcRnInterfaceError $
                 Can'tFindInterface err $
                 LookingForModule mod want_boot
           Succeeded iface ->
             return iface
       }

-- | Like 'loadSrcInterface', but returns a 'MaybeErr'.
loadSrcInterface_maybe :: SDoc
                       -> ModuleName
                       -> IsBootInterface     -- {-# SOURCE #-} ?
                       -> PkgQual             -- "package", if any
                       -> RnM (MaybeErr MissingInterfaceError ModIface)

loadSrcInterface_maybe doc mod want_boot maybe_pkg
  -- We must first find which Module this import refers to.  This involves
  -- calling the Finder, which as a side effect will search the filesystem
  -- and create a ModLocation.  If successful, loadIface will read the
  -- interface; it will call the Finder again, but the ModLocation will be
  -- cached from the first search.
  = do hsc_env <- getTopEnv
       res <- liftIO $ findImportedModule hsc_env mod maybe_pkg
       case res of
           Found _ mod -> initIfaceTcRn $ loadInterface doc mod (ImportByUser want_boot)
           -- TODO: Make sure this error message is good
           err         -> return (Failed (cannotFindModule hsc_env mod err))

-- | Load interface directly for a fully qualified 'Module'.  (This is a fairly
-- rare operation, but in particular it is used to load orphan modules
-- in order to pull their instances into the global package table and to
-- handle some operations in GHCi).
loadModuleInterface :: SDoc -> Module -> TcM ModIface
loadModuleInterface doc mod = initIfaceTcRn (loadSysInterface doc mod)

-- | Load interfaces for a collection of modules.
loadModuleInterfaces :: SDoc -> [Module] -> TcM ()
loadModuleInterfaces doc mods
  | null mods = return ()
  | otherwise = initIfaceTcRn (mapM_ load mods)
  where
    load mod = loadSysInterface (doc <+> parens (ppr mod)) mod

-- | Loads the interface for a given Name.
-- Should only be called for an imported name;
-- otherwise loadSysInterface may not find the interface
loadInterfaceForName :: SDoc -> Name -> TcRn ModIface
loadInterfaceForName doc name
  = do { when debugIsOn $  -- Check pre-condition
         do { this_mod <- getModule
            ; massertPpr (not (nameIsLocalOrFrom this_mod name)) (ppr name <+> parens doc) }
      ; assertPpr (isExternalName name) (ppr name) $
        initIfaceTcRn $ loadSysInterface doc (nameModule name) }

-- | Loads the interface for a given Module.
loadInterfaceForModule :: SDoc -> Module -> TcRn ModIface
loadInterfaceForModule doc m
  = do
    -- Should not be called with this module
    when debugIsOn $ do
      this_mod <- getModule
      massertPpr (this_mod /= m) (ppr m <+> parens doc)
    initIfaceTcRn $ loadSysInterface doc m

{-
*********************************************************
*                                                      *
                loadInterface

        The main function to load an interface
        for an imported module, and put it in
        the External Package State
*                                                      *
*********************************************************
-}

-- | An 'IfM' function to load the home interface for a wired-in thing,
-- so that we're sure that we see its instance declarations and rules
-- See Note [Loading instances for wired-in things]
loadWiredInHomeIface :: Name -> IfM lcl ()
loadWiredInHomeIface name
  = assert (isWiredInName name) $
    do _ <- loadSysInterface doc (nameModule name); return ()
  where
    doc = text "Need home interface for wired-in thing" <+> ppr name

------------------
-- | Loads a system interface and throws an exception if it fails
loadSysInterface :: SDoc -> Module -> IfM lcl ModIface
loadSysInterface doc mod_name = loadInterfaceWithException doc mod_name ImportBySystem

------------------
-- | Loads a user interface and throws an exception if it fails. The first parameter indicates
-- whether we should import the boot variant of the module
loadUserInterface :: IsBootInterface -> SDoc -> Module -> IfM lcl ModIface
loadUserInterface is_boot doc mod_name
  = loadInterfaceWithException doc mod_name (ImportByUser is_boot)

loadPluginInterface :: SDoc -> Module -> IfM lcl ModIface
loadPluginInterface doc mod_name
  = loadInterfaceWithException doc mod_name ImportByPlugin

------------------
-- | A wrapper for 'loadInterface' that throws an exception if it fails
loadInterfaceWithException :: SDoc -> Module -> WhereFrom -> IfM lcl ModIface
loadInterfaceWithException doc mod_name where_from
  = do
    dflags <- getDynFlags
    let ctx = initSDocContext dflags defaultUserStyle
    withIfaceErr ctx (loadInterface doc mod_name where_from)

-- | Load the part of the external module graph which is transitively reachable
-- from the given modules.
--
-- This operation is used just before TH splices are run (in 'getLinkDeps').
--
-- A field in the EPS tracks which home modules are already fully loaded, which we use
-- here to avoid trying to load them a second time.
--
-- The function takes a set of keys which are currently in the process of being loaded.
-- This is used to avoid duplicating work by loading keys twice if they appear along multiple
-- paths in the transitive closure. Once the interface and all its dependencies are
-- loaded, the key is added to the "fully loaded" set, so we know that it and it's
-- transitive closure are present in the graph.
--
-- Note that being "in progress" is different from being "fully loaded", consider if there
-- is an exception during `loadExternalGraphBelow`, then an "in progress" item may fail
-- to become fully loaded.
loadExternalGraphBelow :: (Module -> SDoc) -> Maybe HomeUnit {-^ The current home unit -}
                               -> Set.Set ExternalKey -> [Module] -> IfM lcl (Set.Set ExternalKey)
loadExternalGraphBelow _ Nothing _ _ = panic "loadExternalGraphBelow: No home unit"
loadExternalGraphBelow msg (Just home_unit) in_progress mods =
  foldM (loadExternalGraphModule msg home_unit) in_progress mods

-- | Load the interface for a module, and all its transitive dependencies but
-- only if we haven't fully loaded the module already or are in the process of fully loading it.
loadExternalGraphModule :: (Module -> SDoc) -> HomeUnit
                         -> Set.Set ExternalKey
                         -> Module
                         -> IfM lcl (Set.Set ExternalKey)
loadExternalGraphModule msg home_unit in_progress mod
  | homeUnitId home_unit /= moduleUnitId mod = do
      loadExternalPackageBelow in_progress (moduleUnitId mod)
  | otherwise =  do

      let key = ExternalModuleKey $ ModNodeKeyWithUid (GWIB (moduleName mod) NotBoot) (moduleUnitId mod)
      graph <- eps_module_graph <$> getEps

      if (not (isFullyLoadedModule key graph || Set.member key in_progress))
        then actuallyLoadExternalGraphModule msg home_unit in_progress key mod
        else return in_progress

-- | Load the interface for a module, and all its transitive dependenices.
actuallyLoadExternalGraphModule
  :: (Module -> SDoc)
  -> HomeUnit
  -> Set.Set ExternalKey
  -> ExternalKey
  -> Module
  -> IOEnv (Env IfGblEnv lcl) (Set.Set ExternalKey)
actuallyLoadExternalGraphModule msg home_unit in_progress key mod = do
  dflags <- getDynFlags
  let ctx = initSDocContext dflags defaultUserStyle
  iface <- withIfaceErr ctx $
    loadInterface (msg mod) mod (ImportByUser NotBoot)

  let deps = mi_deps iface
      mod_deps = dep_direct_mods deps
      pkg_deps = dep_direct_pkgs deps

  -- Do not attempt to load the same key again when traversing
  let in_progress' = Set.insert key in_progress

  -- Load all direct dependencies that are in the home package
  cache_mods <- loadExternalGraphBelow msg (Just home_unit) in_progress'
    $ map (\(uid, GWIB mn _) -> mkModule (RealUnit (Definite uid)) mn)
    $ Set.toList mod_deps

  -- Load all the package nodes, and packages beneath them.
  cache_pkgs <- foldM loadExternalPackageBelow cache_mods (Set.toList pkg_deps)

  registerFullyLoaded key
  return cache_pkgs

registerFullyLoaded :: ExternalKey -> IfM lcl ()
registerFullyLoaded key = do
    -- Update the external graph with this module being fully loaded.
    logger <- getLogger
    liftIO $ trace_if logger (text "Fully loaded:" <+> ppr key)
    updateEps_ $ \eps ->
      eps{eps_module_graph = setFullyLoadedModule key (eps_module_graph eps)}

loadExternalPackageBelow :: Set.Set ExternalKey -> UnitId ->  IfM lcl (Set.Set ExternalKey)
loadExternalPackageBelow in_progress uid = do
    graph <- eps_module_graph <$> getEps
    us    <- hsc_units <$> getTopEnv
    let key = ExternalPackageKey uid
    if not (isFullyLoadedModule key graph || Set.member key in_progress)
      then do
        let in_progress' = Set.insert key in_progress
        case unitDepends <$> lookupUnitId us uid of
          Just dep_uids -> do
            loadPackageIntoEPSGraph uid dep_uids
            final_cache <- foldM loadExternalPackageBelow in_progress' dep_uids
            registerFullyLoaded key
            return final_cache
          Nothing -> pprPanic "loadExternalPackagesBelow: missing" (ppr uid)
      else
        return in_progress

loadPackageIntoEPSGraph :: UnitId -> [UnitId] -> IfM lcl ()
loadPackageIntoEPSGraph uid dep_uids =
  updateEps_ $ \eps ->
    eps { eps_module_graph =
      extendExternalModuleGraph (NodeExternalPackage uid
        (Set.fromList dep_uids)) (eps_module_graph eps) }

------------------
loadInterface :: SDoc -> Module -> WhereFrom
              -> IfM lcl (MaybeErr MissingInterfaceError ModIface)

-- loadInterface looks in both the HPT and PIT for the required interface
-- If not found, it loads it, and puts it in the PIT (always).

-- If it can't find a suitable interface file, we
--      a) modify the PackageIfaceTable to have an empty entry
--              (to avoid repeated complaints)
--      b) return (Left message)
--
-- It's not necessarily an error for there not to be an interface
-- file -- perhaps the module has changed, and that interface
-- is no longer used

loadInterface doc_str mod from
  | isHoleModule mod
  -- Hole modules get special treatment
  = do hsc_env <- getTopEnv
       let home_unit = hsc_home_unit hsc_env
       -- Redo search for our local hole module
       loadInterface doc_str (mkHomeModule home_unit (moduleName mod)) from
  | otherwise
  = do
    logger <- getLogger
    withTimingSilent logger (text "loading interface") (pure ()) $ do
        {       -- Read the state
          (eps,hug) <- getEpsAndHug
        ; gbl_env <- getGblEnv

        ; liftIO $ trace_if logger (text "Considering whether to load" <+> ppr mod <+> ppr from)

                -- Check whether we have the interface already
        ; hsc_env <- getTopEnv
        ; let mhome_unit = ue_homeUnit (hsc_unit_env hsc_env)
        ; liftIO (lookupIfaceByModule hug (eps_PIT eps) mod) >>= \case {
            Just iface
                -> return (Succeeded iface) ;   -- Already loaded
            _ -> do {

        -- READ THE MODULE IN
        ; read_result <- case wantHiBootFile mhome_unit eps mod from of
                           Failed err             -> return (Failed err)
                           Succeeded hi_boot_file -> do
                             hsc_env <- getTopEnv
                             liftIO $ computeInterface hsc_env doc_str hi_boot_file mod
        ; case read_result of {
            Failed err -> do
                { let fake_iface = emptyFullModIface mod

                ; updateEps_ $ \eps ->
                        eps { eps_PIT = extendModuleEnv (eps_PIT eps) (mi_module fake_iface) fake_iface }
                        -- Not found, so add an empty iface to
                        -- the EPS map so that we don't look again

                ; return (Failed err) } ;

        -- Found and parsed!
        -- We used to have a sanity check here that looked for:
        --  * System importing ..
        --  * a home package module ..
        --  * that we know nothing about (mb_dep == Nothing)!
        --
        -- But this is no longer valid because thNameToGhcName allows users to
        -- cause the system to load arbitrary interfaces (by supplying an appropriate
        -- Template Haskell original-name).
            Succeeded (iface, loc) ->
        let
            loc_doc = text (ml_hi_file loc)
        in
        initIfaceLcl (mi_semantic_module iface) loc_doc (mi_boot iface) $

        dontLeakTheHUG $ do

        --      Load the new ModIface into the External Package State
        -- Even home-package interfaces loaded by loadInterface
        --      (which only happens in OneShot mode; in Batch/Interactive
        --      mode, home-package modules are loaded one by one into the HPT)
        -- are put in the EPS.
        --
        -- The main thing is to add the ModIface to the PIT, but
        -- we also take the
        --      IfaceDecls, IfaceClsInst, IfaceFamInst, IfaceRules,
        -- out of the ModIface and put them into the big EPS pools

        -- NB: *first* we do tcIfaceDecls, so that the provenance of all the locally-defined
        ---    names is done correctly (notably, whether this is an .hi file or .hi-boot file).
        --     If we do loadExport first the wrong info gets into the cache (unless we
        --      explicitly tag each export which seems a bit of a bore)

        -- Crucial assertion that checks if you are trying to load a HPT module into the EPS.
        -- If you start loading HPT modules into the EPS then you get strange errors about
        -- overlapping instances.
        ; massertPpr
              ((isOneShot (ghcMode (hsc_dflags hsc_env)))
                || moduleUnitId mod `notElem` hsc_all_home_unit_ids hsc_env
                || mod == gHC_PRIM)
                (text "Attempting to load home package interface into the EPS" $$ ppr (HUG.allUnits hug) $$ doc_str $$ ppr mod $$ ppr (moduleUnitId mod))
        ; ignore_prags      <- goptM Opt_IgnoreInterfacePragmas
        ; new_eps_decls     <- tcIfaceDecls ignore_prags (mi_decls iface)
        ; new_eps_insts     <- mapM tcIfaceInst (mi_insts iface)
        ; new_eps_fam_insts <- mapM tcIfaceFamInst (mi_fam_insts iface)
        ; new_eps_rules     <- tcIfaceRules ignore_prags (mi_rules iface)
        ; new_eps_anns      <- tcIfaceAnnotations (mi_anns iface)
        ; new_eps_complete_matches <- tcIfaceCompleteMatches (mi_complete_matches iface)
        ; purged_hsc_env <- getTopEnv

        ; let direct_deps = map (uncurry (flip ModNodeKeyWithUid)) $ (Set.toList (dep_direct_mods $ mi_deps iface))
        ; let direct_pkg_deps = Set.toList $ dep_direct_pkgs $ mi_deps iface
        ; let !module_graph_key =
                if moduleUnitId mod `elem` hsc_all_home_unit_ids hsc_env
                                    --- ^ home unit mods in eps can only happen in oneshot mode
                  then Just $ NodeHomePackage (miKey iface) (map ExternalModuleKey direct_deps
                                                            ++ map ExternalPackageKey direct_pkg_deps)
                  else Nothing

        ; let final_iface = iface
                               & set_mi_decls     (panic "No mi_decls in PIT")
                               & set_mi_insts     (panic "No mi_insts in PIT")
                               & set_mi_fam_insts (panic "No mi_fam_insts in PIT")
                               & set_mi_rules     (panic "No mi_rules in PIT")
                               & set_mi_anns      (panic "No mi_anns in PIT")
                               & set_mi_extra_decls (panic "No mi_extra_decls in PIT")

              bad_boot = mi_boot iface == IsBoot
                          && isJust (lookupKnotVars (if_rec_types gbl_env) mod)
                            -- Warn against an EPS-updating import
                            -- of one's own boot file! (one-shot only)
                            -- See Note [Loading your own hi-boot file]

              -- Create an IO action that loads and compiles bytecode from Core
              -- bindings.
              --
              -- See Note [Interface Files with Core Definitions]
              add_bytecode old
                | Just action <- loadIfaceByteCode purged_hsc_env iface loc (mkNameEnv new_eps_decls)
                = extendModuleEnv old mod action
                -- Don't add an entry if the iface doesn't have 'extra_decls'
                -- so 'get_link_deps' knows that it should load object code.
                | otherwise
                = old

        ; warnPprTrace bad_boot "loadInterface" (ppr mod) $
          updateEps_  $ \ eps ->
           if elemModuleEnv mod (eps_PIT eps) || is_external_sig mhome_unit iface
                then eps
           else if bad_boot
                -- See Note [Loading your own hi-boot file]
                then eps { eps_PTE = addDeclsToPTE (eps_PTE eps) new_eps_decls }
           else
                eps {
                  eps_PIT          = extendModuleEnv (eps_PIT eps) mod final_iface,
                  eps_PTE          = addDeclsToPTE   (eps_PTE eps) new_eps_decls,
                  eps_iface_bytecode = add_bytecode (eps_iface_bytecode eps),
                  eps_rule_base    = extendRuleBaseList (eps_rule_base eps)
                                                        new_eps_rules,
                  eps_module_graph =
                    let eps_graph'  = case module_graph_key of
                                       Just k -> extendExternalModuleGraph k (eps_module_graph eps)
                                       Nothing -> eps_module_graph eps
                     in eps_graph',
                  eps_complete_matches
                                   = eps_complete_matches eps ++ new_eps_complete_matches,
                  eps_inst_env     = extendInstEnvList (eps_inst_env eps)
                                                       new_eps_insts,
                  eps_fam_inst_env = extendFamInstEnvList (eps_fam_inst_env eps)
                                                          new_eps_fam_insts,
                  eps_ann_env      = extendAnnEnvList (eps_ann_env eps)
                                                      new_eps_anns,
                  eps_mod_fam_inst_env
                                   = let
                                       fam_inst_env =
                                         extendFamInstEnvList emptyFamInstEnv
                                                              new_eps_fam_insts
                                     in
                                     extendModuleEnv (eps_mod_fam_inst_env eps)
                                                     mod
                                                     fam_inst_env,
                  eps_stats        = addEpsInStats (eps_stats eps)
                                                   (length new_eps_decls)
                                                   (length new_eps_insts)
                                                   (length new_eps_rules) }

        ; -- invoke plugins with *full* interface, not final_iface, to ensure
          -- that plugins have access to declarations, etc.
          res <- withPlugins (hsc_plugins hsc_env) (\p -> interfaceLoadAction p) iface
        ; return (Succeeded res)
    }}}}

{- Note [Loading your own hi-boot file]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generally speaking, when compiling module M, we should not
load M.hi-boot into the EPS.  After all, we are very shortly
going to have full information about M.  Moreover, see
Note [Do not update EPS with your own hi-boot] in GHC.Iface.Recomp.

But there is a HORRIBLE HACK here.

* At the end of tcRnImports, we call checkFamInstConsistency to
  check consistency of imported type-family instances
  See Note [The type family instance consistency story] in GHC.Tc.Instance.Family

* Alas, those instances may refer to data types defined in M,
  if there is a M.hs-boot.

* And that means we end up loading M.hi-boot, because those
  data types are not yet in the type environment.

But in this weird case, /all/ we need is the types. We don't need
instances, rules etc.  And if we put the instances in the EPS
we get "duplicate instance" warnings when we compile the "real"
instance in M itself.  Hence the strange business of just updateing
the eps_PTE.

This really happens in practice.  The module "GHC.Hs.Expr" gets
"duplicate instance" errors if this hack is not present.

This is a mess.


Note [Home Unit Graph space leak]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ticket: #15111

In IfL, we defer some work until it is demanded using forkM, such
as building TyThings from IfaceDecls. These thunks are stored in
the ExternalPackageState, and they might never be poked.  If we're
not careful, these thunks will capture the state of the loaded
program when we read an interface file, and retain all that data
for ever.

Therefore, when loading a package interface file , we use a "clean"
version of the HscEnv with all the data about the currently loaded
program stripped out. Most of the fields can be panics because
we'll never read them, but hsc_HUG needs to be empty because this
interface will cause other interfaces to be loaded recursively, and
when looking up those interfaces we use the HUG in loadInterface.
We know that none of the interfaces below here can refer to
home-package modules however, so it's safe for the HUG to be empty.
-}

-- Note [GHC Heap Invariants]
-- Note [Home Unit Graph space leak]
dontLeakTheHUG :: IfL a -> IfL a
dontLeakTheHUG thing_inside = do
  env <- getTopEnv
  let
    inOneShot =
      isOneShot (ghcMode (hsc_dflags env))
    cleanGblEnv gbl_env
      | inOneShot = gbl_env
      | otherwise = gbl_env { if_rec_types = emptyKnotVars }
    cleanTopEnv hsc_env =

      let
        !maybe_type_vars | inOneShot = Just (hsc_type_env_vars env)
                         | otherwise = Nothing
        -- wrinkle: when we're typechecking in --backpack mode, the
        -- instantiation of a signature might reside in the HPT, so
        -- this case breaks the assumption that EPS interfaces only
        -- refer to other EPS interfaces.
        -- As a temporary (MP Oct 2021 #20509) we only keep the HPT if it
        -- contains any hole modules.
        -- Quite a few tests in testsuite/tests/backpack break without this
        -- tweak.
        old_unit_env = hsc_unit_env hsc_env
        keepFor20509
         | mgHasHoles (hsc_mod_graph hsc_env) = True
         | otherwise = False
        pruneHomeUnitEnv hme = do
          -- NB: These are empty HPTs because Iface/Load first consults the HPT
          emptyHPT <- liftIO emptyHomePackageTable
          return $! hme{ homeUnitEnv_hpt = emptyHPT }
        unit_env_io
          | keepFor20509
          = return old_unit_env
          | otherwise
          = do
            hug' <- traverse pruneHomeUnitEnv (ue_home_unit_graph old_unit_env)
            return old_unit_env
              { ue_home_unit_graph = hug'
              }
      in do
        !unit_env <- unit_env_io
        -- mg_has_holes will be checked again, but nothing else about the module graph
        let !new_mod_graph = emptyMG { mg_mss = panic "cleanTopEnv: mg_mss"
                                     , mg_graph = panic "cleanTopEnv: mg_graph"
                                     , mg_has_holes = keepFor20509 }
        pure $
          hsc_env
                {  hsc_targets      = panic "cleanTopEnv: hsc_targets"
                ,  hsc_mod_graph    = new_mod_graph
                ,  hsc_IC           = panic "cleanTopEnv: hsc_IC"
                ,  hsc_type_env_vars = case maybe_type_vars of
                                           Just vars -> vars
                                           Nothing -> panic "cleanTopEnv: hsc_type_env_vars"
                ,  hsc_unit_env     = unit_env
                }

  updTopEnvIO cleanTopEnv $ updGblEnv cleanGblEnv $ do
  !_ <- getTopEnv        -- force the updTopEnv
  !_ <- getGblEnv
  thing_inside


-- | Returns @True@ if a 'ModIface' comes from an external package.
-- In this case, we should NOT load it into the EPS; the entities
-- should instead come from the local merged signature interface.
is_external_sig :: Maybe HomeUnit -> ModIface -> Bool
is_external_sig mhome_unit iface =
    -- It's a signature iface...
    mi_semantic_module iface /= mi_module iface &&
    -- and it's not from the local package
    notHomeModuleMaybe mhome_unit (mi_module iface)

-- | This is an improved version of 'findAndReadIface' which can also
-- handle the case when a user requests @p[A=<B>]:M@ but we only
-- have an interface for @p[A=<A>]:M@ (the indefinite interface.
-- If we are not trying to build code, we load the interface we have,
-- *instantiating it* according to how the holes are specified.
-- (Of course, if we're actually building code, this is a hard error.)
--
-- In the presence of holes, 'computeInterface' has an important invariant:
-- to load module M, its set of transitively reachable requirements must
-- have an up-to-date local hi file for that requirement.  Note that if
-- we are loading the interface of a requirement, this does not
-- apply to the requirement itself; e.g., @p[A=<A>]:A@ does not require
-- A.hi to be up-to-date (and indeed, we MUST NOT attempt to read A.hi, unless
-- we are actually typechecking p.)
computeInterface
  :: HscEnv
  -> SDoc
  -> IsBootInterface
  -> Module
  -> IO (MaybeErr MissingInterfaceError (ModIface, ModLocation))
computeInterface hsc_env doc_str hi_boot_file mod0 = do
  massert (not (isHoleModule mod0))
  let mhome_unit  = hsc_home_unit_maybe hsc_env
  let find_iface m = findAndReadIface hsc_env doc_str
                                      m mod0 hi_boot_file
  case getModuleInstantiation mod0 of
      (imod, Just indef)
        | Just home_unit <- mhome_unit
        , isHomeUnitIndefinite home_unit ->
          find_iface imod >>= \case
            Succeeded (iface0, path) ->
              rnModIface hsc_env (instUnitInsts (moduleUnit indef)) Nothing iface0 >>= \case
                Right x   -> return (Succeeded (x, path))
                Left errs -> throwErrors (GhcTcRnMessage <$> errs)
            Failed err -> return (Failed err)
      (mod, _) -> find_iface mod

-- | Compute the signatures which must be compiled in order to
-- load the interface for a 'Module'.  The output of this function
-- is always a subset of 'moduleFreeHoles'; it is more precise
-- because in signature @p[A=\<A>,B=\<B>]:B@, although the free holes
-- are A and B, B might not depend on A at all!
--
-- If this is invoked on a signature, this does NOT include the
-- signature itself; e.g. precise free module holes of
-- @p[A=\<A>,B=\<B>]:B@ never includes B.
moduleFreeHolesPrecise
    :: SDoc -> Module
    -> TcRnIf gbl lcl (MaybeErr MissingInterfaceError (UniqDSet ModuleName))
moduleFreeHolesPrecise doc_str mod
 | moduleIsDefinite mod = return (Succeeded emptyUniqDSet)
 | otherwise =
   case getModuleInstantiation mod of
    (imod, Just indef) -> do
        logger <- getLogger
        let insts = instUnitInsts (moduleUnit indef)
        liftIO $ trace_if logger (text "Considering whether to load" <+> ppr mod <+>
                 text "to compute precise free module holes")
        (eps, hpt) <- getEpsAndHug
        result <- tryEpsAndHpt eps hpt
        case result `firstJust` tryDepsCache eps imod insts of
          Just r -> return (Succeeded r)
          Nothing -> readAndCache imod insts
    (_, Nothing) -> return (Succeeded emptyUniqDSet)
  where
    tryEpsAndHpt eps hpt =
        fmap mi_free_holes <$> liftIO (lookupIfaceByModule hpt (eps_PIT eps) mod)
    tryDepsCache eps imod insts =
        case lookupInstalledModuleEnv (eps_free_holes eps) imod of
            Just ifhs  -> Just (renameFreeHoles ifhs insts)
            _otherwise -> Nothing
    readAndCache imod insts = do
        hsc_env <- getTopEnv
        mb_iface <- liftIO $ findAndReadIface hsc_env
                                              (text "moduleFreeHolesPrecise" <+> doc_str)
                                              imod mod NotBoot
        case mb_iface of
            Succeeded (iface, _) -> do
                let ifhs = mi_free_holes iface
                -- Cache it
                updateEps_ (\eps ->
                    eps { eps_free_holes = extendInstalledModuleEnv (eps_free_holes eps) imod ifhs })
                return (Succeeded (renameFreeHoles ifhs insts))
            Failed err -> return (Failed err)

wantHiBootFile :: Maybe HomeUnit -> ExternalPackageState -> Module -> WhereFrom
               -> MaybeErr MissingInterfaceError IsBootInterface
-- Figure out whether we want Foo.hi or Foo.hi-boot
wantHiBootFile mhome_unit eps mod from
  = case from of
       ImportByUser usr_boot
          | usr_boot == IsBoot && notHomeModuleMaybe mhome_unit mod
          -> Failed (BadSourceImport mod)
          | otherwise -> Succeeded usr_boot

       ImportByPlugin
          -> Succeeded NotBoot

       ImportBySystem
          | notHomeModuleMaybe mhome_unit mod
          -> Succeeded NotBoot
             -- If the module to be imported is not from this package
             -- don't look it up in eps_is_boot, because that is keyed
             -- on the ModuleName of *home-package* modules only.
             -- We never import boot modules from other packages!

          | otherwise
          -> case lookupInstalledModuleEnv (eps_is_boot eps) (toUnitId <$> mod) of
                Just (GWIB { gwib_isBoot = is_boot }) ->
                  Succeeded is_boot
                Nothing ->
                  Succeeded NotBoot
                     -- The boot-ness of the requested interface,
                     -- based on the dependencies in directly-imported modules


-----------------------------------------------------
--      Loading type/class/value decls
-- We pass the full Module name here, replete with
-- its package info, so that we can build a Name for
-- each binder with the right package info in it
-- All subsequent lookups, including crucially lookups during typechecking
-- the declaration itself, will find the fully-glorious Name
--
-- We handle ATs specially.  They are not main declarations, but also not
-- implicit things (in particular, adding them to `implicitTyThings' would mess
-- things up in the renaming/type checking of source programs).
-----------------------------------------------------

addDeclsToPTE :: PackageTypeEnv -> [(Name,TyThing)] -> PackageTypeEnv
addDeclsToPTE pte things = extendNameEnvList pte things

{-
*********************************************************
*                                                      *
\subsection{Reading an interface file}
*                                                      *
*********************************************************

Note [Home module load error]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the sought-for interface is in the current package (as determined
by -package-name flag) then it jolly well should already be in the HPT
because we process home-package modules in dependency order.  (Except
in one-shot mode; see notes with hsc_HPT decl in GHC.Driver.Env).

It is possible (though hard) to get this error through user behaviour.
  * Suppose package P (modules P1, P2) depends on package Q (modules Q1,
    Q2, with Q2 importing Q1)
  * We compile both packages.
  * Now we edit package Q so that it somehow depends on P
  * Now recompile Q with --make (without recompiling P).
  * Then Q1 imports, say, P1, which in turn depends on Q2. So Q2
    is a home-package module which is not yet in the HPT!  Disaster.

This actually happened with P=base, Q=ghc-prim, via the AMP warnings.
See #8320.
-}

findAndReadIface
  :: HscEnv
  -> SDoc            -- ^ Reason for loading the iface (used for tracing)
  -> InstalledModule -- ^ The unique identifier of the on-disk module we're looking for
  -> Module          -- ^ The *actual* module we're looking for.  We use
                     -- this to check the consistency of the requirements of the
                     -- module we read out.
  -> IsBootInterface -- ^ Looking for .hi-boot or .hi file
  -> IO (MaybeErr MissingInterfaceError (ModIface, ModLocation))
findAndReadIface hsc_env doc_str mod wanted_mod hi_boot_file = do

  let profile = targetProfile dflags
      unit_state = hsc_units hsc_env
      fc         = hsc_FC hsc_env
      name_cache = hsc_NC hsc_env
      mhome_unit  = hsc_home_unit_maybe hsc_env
      dflags     = hsc_dflags hsc_env
      logger     = hsc_logger hsc_env
      other_fopts = initFinderOpts . homeUnitEnv_dflags <$> (hsc_HUG hsc_env)


  trace_if logger (sep [hsep [text "Reading",
                           if hi_boot_file == IsBoot
                             then text "[boot]"
                             else Outputable.empty,
                           text "interface for",
                           ppr mod <> semi],
                     nest 4 (text "reason:" <+> doc_str)])

  -- Check for GHC.Prim, and return its static interface
  -- See Note [GHC.Prim] in primops.txt.pp.
  -- TODO: make this check a function
  if mod `installedModuleEq` gHC_PRIM
      then do
          let iface = getGhcPrimIface hsc_env
          return (Succeeded (iface, panic "GHC.Prim ModLocation (findAndReadIface)"))
      else do
          let fopts = initFinderOpts dflags
          -- Look for the file
          mb_found <- liftIO (findExactModule fc fopts other_fopts unit_state mhome_unit mod hi_boot_file)
          case mb_found of
              InstalledFound loc -> do
                  -- See Note [Home module load error]
                  case mhome_unit of
                    Just home_unit
                      | isHomeInstalledModule home_unit mod
                      , not (isOneShot (ghcMode dflags))
                      -> return (Failed (HomeModError mod loc))
                    _ -> do
                        r <- read_file logger name_cache unit_state dflags wanted_mod (ml_hi_file loc)
                        case r of
                          Failed err
                            -> return (Failed $ BadIfaceFile err)
                          Succeeded (iface,_fp)
                            -> do
                                r2 <- load_dynamic_too_maybe logger name_cache unit_state
                                                         (setDynamicNow dflags) wanted_mod
                                                         iface loc
                                case r2 of
                                  Failed sdoc -> return (Failed sdoc)
                                  Succeeded {} -> return $ Succeeded (iface, loc)
              err -> do
                  trace_if logger (text "...not found")
                  return $ Failed $ cannotFindInterface
                                      unit_state
                                      mhome_unit
                                      profile
                                      (moduleName mod)
                                      err

-- | Check if we need to try the dynamic interface for -dynamic-too
load_dynamic_too_maybe :: Logger -> NameCache -> UnitState -> DynFlags
                       -> Module -> ModIface -> ModLocation
                       -> IO (MaybeErr MissingInterfaceError ())
load_dynamic_too_maybe logger name_cache unit_state dflags wanted_mod iface loc
  -- Indefinite interfaces are ALWAYS non-dynamic.
  | not (moduleIsDefinite (mi_module iface)) = return (Succeeded ())
  | gopt Opt_BuildDynamicToo dflags = load_dynamic_too logger name_cache unit_state dflags wanted_mod iface loc
  | otherwise = return (Succeeded ())

load_dynamic_too :: Logger -> NameCache -> UnitState -> DynFlags
                 -> Module -> ModIface -> ModLocation
                 -> IO (MaybeErr MissingInterfaceError ())
load_dynamic_too logger name_cache unit_state dflags wanted_mod iface loc = do
  read_file logger name_cache unit_state dflags wanted_mod (ml_dyn_hi_file loc) >>= \case
    Succeeded (dynIface, _)
     | mi_mod_hash (mi_final_exts iface) == mi_mod_hash (mi_final_exts dynIface)
     -> return (Succeeded ())
     | otherwise ->
        do return $ (Failed $ DynamicHashMismatchError wanted_mod loc)
    Failed err ->
        do return $ (Failed $ FailedToLoadDynamicInterface wanted_mod err)

          --((text "Failed to load dynamic interface file for" <+> ppr wanted_mod <> colon) $$ err))




read_file :: Logger -> NameCache -> UnitState -> DynFlags
          -> Module -> FilePath
          -> IO (MaybeErr ReadInterfaceError (ModIface, FilePath))
read_file logger name_cache unit_state dflags wanted_mod file_path = do
  trace_if logger (text "readIFace" <+> text file_path)

  -- Figure out what is recorded in mi_module.  If this is
  -- a fully definite interface, it'll match exactly, but
  -- if it's indefinite, the inside will be uninstantiated!
  let wanted_mod' =
        case getModuleInstantiation wanted_mod of
            (_, Nothing) -> wanted_mod
            (_, Just indef_mod) ->
              instModuleToModule unit_state
                (uninstantiateInstantiatedModule indef_mod)
  read_result <- readIface dflags name_cache wanted_mod' file_path
  case read_result of
    Failed err      -> return (Failed err)
    Succeeded iface -> return (Succeeded (iface, file_path))
                -- Don't forget to fill in the package name...


-- | Write interface file
writeIface :: Logger -> Profile -> CompressionIFace -> FilePath -> ModIface -> IO ()
writeIface logger profile compression_level hi_file_path new_iface
    = do createDirectoryIfMissing True (takeDirectory hi_file_path)
         let printer = TraceBinIFace (debugTraceMsg logger 3)
         writeBinIface profile printer compression_level hi_file_path new_iface

flagsToIfCompression :: DynFlags -> CompressionIFace
flagsToIfCompression dflags
  | n <= 1 = NormalCompression
  | n == 2 = SafeExtraCompression
  -- n >= 3
  | otherwise = MaximumCompression
  where n = ifCompression dflags

-- | @readIface@ tries just the one file.
--
-- Failed err    <=> file not found, or unreadable, or illegible
-- Succeeded iface <=> successfully found and parsed
readIface
  :: DynFlags
  -> NameCache
  -> Module
  -> FilePath
  -> IO (MaybeErr ReadInterfaceError ModIface)
readIface dflags name_cache wanted_mod file_path = do
  let profile = targetProfile dflags
  res <- tryMost $ readBinIface profile name_cache CheckHiWay QuietBinIFace file_path
  case res of
    Right iface
        -- NB: This check is NOT just a sanity check, it is
        -- critical for correctness of recompilation checking
        -- (it lets us tell when -this-unit-id has changed.)
        | wanted_mod == actual_mod
                        -> return (Succeeded iface)
        | otherwise     -> return (Failed err)
        where
          actual_mod = mi_module iface
          err = HiModuleNameMismatchWarn file_path wanted_mod actual_mod

    Left exn    -> return (Failed (ExceptionOccurred file_path exn))

{-
*********************************************************
*                                                       *
        Wired-in interface for GHC.Prim
*                                                       *
*********************************************************
-}

-- See Note [GHC.Prim] in primops.txt.pp.
ghcPrimIface :: ModIface
ghcPrimIface
  = empty_iface
      & set_mi_exports  ghcPrimExports
      & set_mi_decls    []
      & set_mi_fixities ghcPrimFixities
      & set_mi_final_exts ((mi_final_exts empty_iface)
          { mi_fix_fn = mkIfaceFixCache ghcPrimFixities
          , mi_decl_warn_fn = mkIfaceDeclWarnCache ghcPrimWarns
          , mi_export_warn_fn = mkIfaceExportWarnCache ghcPrimWarns
          })
      & set_mi_docs (Just ghcPrimDeclDocs) -- See Note [GHC.Prim Docs] in GHC.Builtin.Utils
      & set_mi_warns (toIfaceWarnings ghcPrimWarns) -- See Note [GHC.Prim Deprecations] in GHC.Builtin.Utils

  where
    empty_iface = emptyFullModIface gHC_PRIM

{-
*********************************************************
*                                                      *
\subsection{Statistics}
*                                                      *
*********************************************************
-}

ifaceStats :: ExternalPackageState -> SDoc
ifaceStats eps
  = hcat [text "Renamer stats: ", msg]
  where
    stats = eps_stats eps
    msg = vcat
        [int (n_ifaces_in stats) <+> text "interfaces read",
         hsep [ int (n_decls_out stats), text "type/class/variable imported, out of",
                int (n_decls_in stats), text "read"],
         hsep [ int (n_insts_out stats), text "instance decls imported, out of",
                int (n_insts_in stats), text "read"],
         hsep [ int (n_rules_out stats), text "rule decls imported, out of",
                int (n_rules_in stats), text "read"]
        ]

{-
************************************************************************
*                                                                      *
                Printing interfaces
*                                                                      *
************************************************************************

Note [Name qualification with --show-iface]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to disambiguate between identifiers from different modules, we qualify
all names that don't originate in the current module. In order to keep visual
noise as low as possible, we keep local names unqualified.

For some background on this choice see #15269.
-}

-- | Read binary interface, and print it out
showIface :: Logger -> DynFlags -> UnitState -> NameCache -> FilePath -> IO ()
showIface logger dflags unit_state name_cache filename = do
   let profile = targetProfile dflags
       printer = logMsg logger MCOutput noSrcSpan . withPprStyle defaultDumpStyle

   -- skip the hi way check; we don't want to worry about profiled vs.
   -- non-profiled interfaces, for example.
   iface <- readBinIface profile name_cache IgnoreHiWay (TraceBinIFace printer) filename

   let -- See Note [Name qualification with --show-iface]
       qualifyImportedNames mod _
           | mod == mi_module iface = NameUnqual
           | otherwise              = NameNotInScope1
       name_ppr_ctx = QueryQualify qualifyImportedNames
                                   neverQualifyModules
                                   neverQualifyPackages
                                   alwaysPrintPromTick
   logMsg logger MCDump noSrcSpan
      $ withPprStyle (mkDumpStyle name_ppr_ctx)
      $ pprModIface unit_state iface

-- | Show a ModIface but don't display details; suitable for ModIfaces stored in
-- the EPT.
pprModIfaceSimple :: UnitState -> ModIface -> SDoc
pprModIfaceSimple unit_state iface =
    ppr (mi_module iface)
    $$ pprDeps unit_state (mi_deps iface)
    $$ nest 2 (vcat (map pprExport (mi_exports iface)))

-- | Show a ModIface
--
-- The UnitState is used to pretty-print units
pprModIface :: UnitState -> ModIface -> SDoc
pprModIface unit_state iface
 = vcat [ text "interface"
                <+> ppr (mi_module iface) <+> pp_hsc_src (mi_hsc_src iface)
                <+> (if mi_orphan exts then text "[orphan module]" else Outputable.empty)
                <+> (if mi_finsts exts then text "[family instance module]" else Outputable.empty)
                <+> (if mi_hpc iface then text "[hpc]" else Outputable.empty)
                <+> integer hiVersion
        , nest 2 (text "interface hash:" <+> ppr (mi_iface_hash exts))
        , nest 2 (text "ABI hash:" <+> ppr (mi_mod_hash exts))
        , nest 2 (text "export-list hash:" <+> ppr (mi_exp_hash exts))
        , nest 2 (text "orphan hash:" <+> ppr (mi_orphan_hash exts))
        , nest 2 (text "flag hash:" <+> ppr (mi_flag_hash exts))
        , nest 2 (text "opt_hash:" <+> ppr (mi_opt_hash exts))
        , nest 2 (text "hpc_hash:" <+> ppr (mi_hpc_hash exts))
        , nest 2 (text "plugin_hash:" <+> ppr (mi_plugin_hash exts))
        , nest 2 (text "src_hash:" <+> ppr (mi_src_hash iface))
        , nest 2 (text "sig of:" <+> ppr (mi_sig_of iface))
        , nest 2 (text "used TH splices:" <+> ppr (mi_used_th iface))
        , nest 2 (text "where")
        , text "exports:"
        , nest 2 (vcat (map pprExport (mi_exports iface)))
        , text "defaults:"
        , nest 2 (vcat (map ppr (mi_defaults iface)))
        , pprDeps unit_state (mi_deps iface)
        , vcat (map pprUsage (mi_usages iface))
        , vcat (map pprIfaceAnnotation (mi_anns iface))
        , pprFixities (mi_fixities iface)
        , vcat [ppr ver $$ nest 2 (ppr decl) | (ver,decl) <- mi_decls iface]
        , case mi_extra_decls iface of
            Nothing -> empty
            Just eds -> text "extra decls:"
                          $$ nest 2 (vcat ([ppr bs | bs <- eds]))
        , vcat (map ppr (mi_insts iface))
        , vcat (map ppr (mi_fam_insts iface))
        , vcat (map ppr (mi_rules iface))
        , ppr (mi_warns iface)
        , pprTrustInfo (mi_trust iface)
        , pprTrustPkg (mi_trust_pkg iface)
        , vcat (map ppr (mi_complete_matches iface))
        , text "docs:" $$ nest 2 (ppr (mi_docs iface))
        , text "extensible fields:" $$ nest 2 (pprExtensibleFields (mi_ext_fields iface))
        ]
  where
    exts = mi_final_exts iface
    pp_hsc_src HsBootFile = text "[boot]"
    pp_hsc_src HsigFile   = text "[hsig]"
    pp_hsc_src HsSrcFile  = Outputable.empty

{-
When printing export lists, we print like this:
        Avail   f               f
        AvailTC C [C, x, y]     C(x,y)
        AvailTC C [x, y]        C!(x,y)         -- Exporting x, y but not C
-}

pprExport :: IfaceExport -> SDoc
pprExport (Avail n)      = ppr n
pprExport (AvailTC _ []) = Outputable.empty
pprExport avail@(AvailTC n _) =
    ppr n <> mark <> pp_export (availSubordinateNames avail)
  where
    mark | availExportsDecl avail = Outputable.empty
         | otherwise              = vbar

    pp_export []    = Outputable.empty
    pp_export names = braces (hsep (map ppr names))

pprUsage :: Usage -> SDoc
pprUsage UsagePackageModule{ usg_mod = mod, usg_mod_hash = hash, usg_safe = safe }
  = pprUsageImport mod hash safe
pprUsage UsageHomeModule{ usg_unit_id = unit_id, usg_mod_name = mod_name
                              , usg_mod_hash = hash, usg_safe = safe
                              , usg_exports = exports, usg_entities = entities }
  = pprUsageImport (mkModule unit_id mod_name) hash safe $$
    nest 2 (
        maybe Outputable.empty (\v -> text "exports: " <> ppr v) exports $$
        vcat [ ppr n <+> ppr v | (n,v) <- entities ]
        )
pprUsage usage@UsageFile{}
  = hsep [text "addDependentFile",
          doubleQuotes (ftext (usg_file_path usage)),
          ppr (usg_file_hash usage)]
pprUsage usage@UsageMergedRequirement{}
  = hsep [text "merged", ppr (usg_mod usage), ppr (usg_mod_hash usage)]
pprUsage usage@UsageHomeModuleInterface{}
  = hsep [text "implementation", ppr (usg_mod_name usage)
                               , ppr (usg_unit_id usage)
                               , ppr (usg_iface_hash usage)]

pprUsageImport :: Outputable mod => mod -> Fingerprint -> IsSafeImport -> SDoc
pprUsageImport mod hash safe
  = hsep [ text "import", pp_safe, ppr mod
         , ppr hash ]
    where
        pp_safe | safe      = text "safe"
                | otherwise = text " -/ "

pprFixities :: [(OccName, Fixity)] -> SDoc
pprFixities []    = Outputable.empty
pprFixities fixes = text "fixities" <+> pprWithCommas pprFix fixes
                  where
                    pprFix (occ,fix) = ppr fix <+> ppr occ

pprTrustInfo :: IfaceTrustInfo -> SDoc
pprTrustInfo trust = text "trusted:" <+> ppr trust

pprTrustPkg :: Bool -> SDoc
pprTrustPkg tpkg = text "require own pkg trusted:" <+> ppr tpkg

pprIfaceAnnotation :: IfaceAnnotation -> SDoc
pprIfaceAnnotation (IfaceAnnotation { ifAnnotatedTarget = target, ifAnnotatedValue = serialized })
  = ppr target <+> text "annotated by" <+> ppr serialized

pprExtensibleFields :: ExtensibleFields -> SDoc
pprExtensibleFields (ExtensibleFields fs) = vcat . map pprField $ toList fs
  where
    pprField (name, (BinData size _data)) = text name <+> text "-" <+> ppr size <+> text "bytes"


-- | Reason for loading an interface file
--
-- Used to figure out whether we want to consider loading hi-boot files or not.
data WhereFrom
  = ImportByUser IsBootInterface        -- Ordinary user import (perhaps {-# SOURCE #-})
  | ImportBySystem                      -- Non user import.
  | ImportByPlugin                      -- Importing a plugin.

instance Outputable WhereFrom where
  ppr (ImportByUser IsBoot)                = text "{- SOURCE -}"
  ppr (ImportByUser NotBoot)               = empty
  ppr ImportBySystem                       = text "{- SYSTEM -}"
  ppr ImportByPlugin                       = text "{- PLUGIN -}"


-- | Get gHC_PRIM interface file
--
-- This is a helper function that takes into account the hook allowing ghc-prim
-- interface to be extended via the ghc-api. Afaik it was introduced for GHCJS
-- so that it can add its own primitive types.
getGhcPrimIface :: HscEnv -> ModIface
getGhcPrimIface hsc_env =
  case ghcPrimIfaceHook (hsc_hooks hsc_env) of
    Nothing -> ghcPrimIface
    Just h  -> h
