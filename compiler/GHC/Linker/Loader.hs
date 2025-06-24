{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

--
--  (c) The University of Glasgow 2002-2006

-- | The loader
--
-- This module deals with the top-level issues of dynamic linking (loading),
-- calling the object-code linker and the byte-code linker where necessary.
module GHC.Linker.Loader
   ( Loader (..)
   , LoaderState (..)
   , initLoaderState
   , uninitializedLoader
   , showLoaderState
   , getLoaderState
   -- * Load & Unload
   , loadDecls
   , loadPackages
   , loadModule
   , loadCmdLineLibs
   , loadName
   , unload
   -- * LoadedEnv
   , withExtendedLoadedEnv
   , extendLoadedEnv
   , deleteFromLoadedEnv
   -- * Internals
   , rmDupLinkables
   , modifyLoaderState
   , initLinkDepsOpts
   , getGccSearchDirectory
   )
where

import GHC.Prelude

import GHC.Settings

import GHC.Platform
import GHC.Platform.Ways

import GHC.Driver.Phases
import GHC.Driver.Env
import GHC.Driver.Session
import GHC.Driver.Ppr
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Config.Finder

import GHC.Tc.Utils.Monad

import GHC.Runtime.Interpreter
import GHCi.BreakArray
import GHCi.RemoteTypes
import GHC.Iface.Load
import GHCi.Message (ConInfoTable(..), LoadedDLL)

import GHC.ByteCode.Linker
import GHC.ByteCode.Asm
import GHC.ByteCode.Types

import GHC.Stack.CCS
import GHC.SysTools

import GHC.Types.Basic
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.SrcLoc
import GHC.Types.Unique.DSet
import GHC.Types.Unique.DFM

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Error
import GHC.Utils.Logger
import GHC.Utils.TmpFs

import GHC.Unit.Env
import GHC.Unit.Home.ModInfo
import GHC.Unit.External (ExternalPackageState (..))
import GHC.Unit.Module
import GHC.Unit.Module.ModNodeKey
import GHC.Unit.Module.Graph
import GHC.Unit.Module.ModIface
import GHC.Unit.State as Packages

import qualified GHC.Data.ShortText as ST
import GHC.Data.FastString

import GHC.Linker.Deps
import GHC.Linker.MacOS
import GHC.Linker.Dynamic
import GHC.Linker.Types

-- Standard libraries
import Control.Monad

import Data.Array
import Data.ByteString (ByteString)
import qualified Data.Set as Set
import Data.Char (isSpace)
import qualified Data.Foldable as Foldable
import Data.IORef
import Data.List (intercalate, isPrefixOf, nub, partition)
import Data.Maybe
import Data.Either
import Control.Concurrent.MVar
import qualified Control.Monad.Catch as MC
import qualified Data.List.NonEmpty as NE

import System.FilePath
import System.Directory
import System.IO.Unsafe
import System.Environment (lookupEnv)

#if defined(mingw32_HOST_OS)
import System.Win32.Info (getSystemDirectory)
#endif

import GHC.Utils.Exception
import GHC.Unit.Home.Graph (lookupHug, unitEnv_foldWithKey)
import GHC.Driver.Downsweep



-- Note [Linkers and loaders]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Linkers are used to produce linked objects (.so, executables); loaders are
-- used to link in memory (e.g., in GHCi) with the already loaded libraries
-- (ghc-lib, rts, etc.).
--
-- Linking can usually be done with an external linker program ("ld"), but
-- loading is more tricky:
--
--    * Fully dynamic:
--       when GHC is built as a set of dynamic libraries (ghc-lib, rts, etc.)
--       and the modules to load are also compiled for dynamic linking, a
--       solution is to fully rely on external tools:
--
--       1) link a .so with the external linker
--       2) load the .so with POSIX's "dlopen"
--
--    * When GHC is built as a static program or when libraries we want to load
--    aren't compiled for dynamic linking, GHC uses its own loader ("runtime
--    linker"). The runtime linker is part of the rts (rts/Linker.c).
--
-- Note that within GHC's codebase we often use the word "linker" to refer to
-- the static object loader in the runtime system.
--
-- Loading can be delegated to an external interpreter ("iserv") when
-- -fexternal-interpreter is used.

uninitialised :: a
uninitialised = panic "Loader not initialised"

modifyLoaderState_ :: Interp -> (LoaderState -> IO LoaderState) -> IO ()
modifyLoaderState_ interp f =
  modifyMVar_ (loader_state (interpLoader interp))
    (fmap pure . f . fromMaybe uninitialised)

modifyLoaderState :: Interp -> (LoaderState -> IO (LoaderState, a)) -> IO a
modifyLoaderState interp f =
  modifyMVar (loader_state (interpLoader interp))
    (fmapFst pure . f . fromMaybe uninitialised)
  where fmapFst f = fmap (\(x, y) -> (f x, y))

getLoaderState :: Interp -> IO (Maybe LoaderState)
getLoaderState interp = readMVar (loader_state (interpLoader interp))


emptyLoaderState :: LoaderState
emptyLoaderState = LoaderState
   { linker_env = LinkerEnv
     { closure_env = emptyNameEnv
     , itbl_env    = emptyNameEnv
     , addr_env    = emptyNameEnv
     , breakarray_env = emptyModuleEnv
     , ccs_env        = emptyModuleEnv
     }
   , pkgs_loaded = init_pkgs
   , bcos_loaded = emptyModuleEnv
   , objs_loaded = emptyModuleEnv
   , temp_sos = []
   }
  -- Packages that don't need loading, because the compiler
  -- shares them with the interpreted program.
  --
  -- The linker's symbol table is populated with RTS symbols using an
  -- explicit list.  See rts/Linker.c for details.
  where init_pkgs = unitUDFM rtsUnitId (LoadedPkgInfo rtsUnitId [] [] [] emptyUniqDSet)

extendLoadedEnv :: Interp -> [(Name,ForeignHValue)] -> IO ()
extendLoadedEnv interp new_bindings =
  modifyLoaderState_ interp $ \pls -> do
    return $! modifyClosureEnv pls $ \ce ->
      extendClosureEnv ce new_bindings
    -- strictness is important for not retaining old copies of the pls

deleteFromLoadedEnv :: Interp -> [Name] -> IO ()
deleteFromLoadedEnv interp to_remove =
  modifyLoaderState_ interp $ \pls -> do
    return $ modifyClosureEnv pls $ \ce ->
      delListFromNameEnv ce to_remove

-- | Load the module containing the given Name and get its associated 'HValue'.
--
-- Throws a 'ProgramError' if loading fails or the name cannot be found.
loadName :: Interp -> HscEnv -> Name -> IO (ForeignHValue, [Linkable], PkgsLoaded)
loadName interp hsc_env name = do
  initLoaderState interp hsc_env
  modifyLoaderState interp $ \pls0 -> do
    (pls, links, pkgs) <- if not (isExternalName name)
       then return (pls0, [], emptyUDFM)
       else do
         (pls', ok, links, pkgs) <- loadDependencies interp hsc_env pls0 noSrcSpan
                                      [nameModule name]
         if failed ok
           then throwGhcExceptionIO (ProgramError "")
           else return (pls', links, pkgs)

    case lookupNameEnv (closure_env (linker_env pls)) name of
      Just (_,aa) -> return (pls,(aa, links, pkgs))
      Nothing     -> assertPpr (isExternalName name) (ppr name) $
                     do let sym_to_find = IClosureSymbol name
                        m <- lookupClosure interp sym_to_find
                        r <- case m of
                          Just hvref -> mkFinalizedHValue interp hvref
                          Nothing -> linkFail "GHC.Linker.Loader.loadName"
                                       (ppr sym_to_find)
                        return (pls,(r, links, pkgs))

loadDependencies
  :: Interp
  -> HscEnv
  -> LoaderState
  -> SrcSpan
  -> [Module]
  -> IO (LoaderState, SuccessFlag, [Linkable], PkgsLoaded) -- ^ returns the set of linkables required
-- When called, the loader state must have been initialized (see `initLoaderState`)
loadDependencies interp hsc_env pls span needed_mods = do
   let opts = initLinkDepsOpts hsc_env

   -- Find what packages and linkables are required
   deps <- getLinkDeps opts interp pls span needed_mods

   let this_pkgs_needed = ldNeededUnits deps

   -- Link the packages and modules required
   pls1 <- loadPackages' interp hsc_env (ldUnits deps) pls
   (pls2, succ) <- loadModuleLinkables interp hsc_env pls1 (ldNeededLinkables deps)
   let this_pkgs_loaded = udfmRestrictKeys all_pkgs_loaded $ getUniqDSet trans_pkgs_needed
       all_pkgs_loaded = pkgs_loaded pls2
       trans_pkgs_needed = unionManyUniqDSets (this_pkgs_needed : [ loaded_pkg_trans_deps pkg
                                                                  | pkg_id <- uniqDSetToList this_pkgs_needed
                                                                  , Just pkg <- [lookupUDFM all_pkgs_loaded pkg_id]
                                                                  ])
   return (pls2, succ, ldAllLinkables deps, this_pkgs_loaded)


-- | Temporarily extend the loaded env.
withExtendedLoadedEnv
  :: (ExceptionMonad m)
  => Interp
  -> [(Name,ForeignHValue)]
  -> m a
  -> m a
withExtendedLoadedEnv interp new_env action
    = MC.bracket (liftIO $ extendLoadedEnv interp new_env)
               (\_ -> reset_old_env)
               (\_ -> action)
    where
        -- Remember that the linker state might be side-effected
        -- during the execution of the IO action, and we don't want to
        -- lose those changes (we might have linked a new module or
        -- package), so the reset action only removes the names we
        -- added earlier.
          reset_old_env = liftIO $
            deleteFromLoadedEnv interp (map fst new_env)


-- | Display the loader state.
showLoaderState :: Interp -> IO SDoc
showLoaderState interp = do
  ls <- readMVar (loader_state (interpLoader interp))
  let docs = case ls of
        Nothing  -> [ text "Loader not initialised"]
        Just pls -> [ text "Pkgs:" <+> ppr (map loaded_pkg_uid $ eltsUDFM $ pkgs_loaded pls)
                    , text "Objs:" <+> ppr (moduleEnvElts $ objs_loaded pls)
                    , text "BCOs:" <+> ppr (moduleEnvElts $ bcos_loaded pls)
                    ]

  return $ withPprStyle defaultDumpStyle
         $ vcat (text "----- Loader state -----":docs)


{- **********************************************************************

                        Initialisation

  ********************************************************************* -}

-- | Initialise the dynamic linker.  This entails
--
--  a) Calling the C initialisation procedure,
--
--  b) Loading any packages specified on the command line,
--
--  c) Loading any packages specified on the command line, now held in the
--     @-l@ options in @v_Opt_l@,
--
--  d) Loading any @.o\/.dll@ files specified on the command line, now held
--     in @ldInputs@,
--
--  e) Loading any MacOS frameworks.
--
-- NOTE: This function is idempotent; if called more than once, it does
-- nothing.  This is useful in Template Haskell, where we call it before
-- trying to link.
--
initLoaderState :: Interp -> HscEnv -> IO ()
initLoaderState interp hsc_env = do
  modifyMVar_ (loader_state (interpLoader interp)) $ \pls -> do
    case pls of
      Just  _ -> return pls
      Nothing -> Just <$> reallyInitLoaderState interp hsc_env

reallyInitLoaderState :: Interp -> HscEnv -> IO LoaderState
reallyInitLoaderState interp hsc_env = do
  -- Initialise the linker state
  let pls0 = emptyLoaderState

  case platformArch (targetPlatform (hsc_dflags hsc_env)) of
    -- FIXME: we don't initialize anything with the JS interpreter.
    -- Perhaps we should load preload packages. We'll load them on demand
    -- anyway.
    ArchJavaScript -> return pls0

    _ -> do
      -- (a) initialise the C dynamic linker
      initObjLinker interp


      -- (b) Load packages from the command-line (Note [preload packages])
      pls <- unitEnv_foldWithKey (\k u env -> k >>= \pls' -> loadPackages' interp (hscSetActiveUnitId u hsc_env) (preloadUnits (homeUnitEnv_units env)) pls') (return pls0) (hsc_HUG hsc_env)

      -- steps (c), (d) and (e)
      loadCmdLineLibs' interp hsc_env pls


loadCmdLineLibs :: Interp -> HscEnv -> IO ()
loadCmdLineLibs interp hsc_env = do
  initLoaderState interp hsc_env
  modifyLoaderState_ interp $ \pls ->
    loadCmdLineLibs' interp hsc_env pls


loadCmdLineLibs' :: Interp -> HscEnv -> LoaderState -> IO LoaderState
loadCmdLineLibs' interp hsc_env pls = snd <$>
    foldM
      (\(done', pls') cur_uid ->  load done' cur_uid pls')
      (Set.empty, pls)
      (hsc_all_home_unit_ids hsc_env)

  where
    load :: Set.Set UnitId -> UnitId -> LoaderState -> IO (Set.Set UnitId, LoaderState)
    load done uid pls | uid `Set.member` done = return (done, pls)
    load done uid pls = do
      let hsc' = hscSetActiveUnitId uid hsc_env
      -- Load potential dependencies first
      (done', pls') <- foldM (\(done', pls') uid -> load done' uid pls') (done, pls)
                          (homeUnitDepends (hsc_units hsc'))
      pls'' <- loadCmdLineLibs'' interp hsc' pls'
      return $ (Set.insert uid done', pls'')

loadCmdLineLibs''
  :: Interp
  -> HscEnv
  -> LoaderState
  -> IO LoaderState
loadCmdLineLibs'' interp hsc_env pls =
  do

      let dflags@(DynFlags { ldInputs = cmdline_ld_inputs
                           , libraryPaths = lib_paths_base})
            = hsc_dflags hsc_env
      let logger = hsc_logger hsc_env

      -- (c) Link libraries from the command-line
      let minus_ls_1 = [ lib | Option ('-':'l':lib) <- cmdline_ld_inputs ]

      -- On Windows we want to add libpthread by default just as GCC would.
      -- However because we don't know the actual name of pthread's dll we
      -- need to defer this to the locateLib call so we can't initialize it
      -- inside of the rts. Instead we do it here to be able to find the
      -- import library for pthreads. See #13210.
      let platform = targetPlatform dflags
          os       = platformOS platform
          minus_ls = case os of
                       OSMinGW32 -> "pthread" : minus_ls_1
                       _         -> minus_ls_1
      -- See Note [Fork/Exec Windows]
      gcc_paths <- getGCCPaths logger dflags os

      lib_paths_env <- addEnvPaths "LIBRARY_PATH" lib_paths_base

      maybePutStrLn logger "Search directories (user):"
      maybePutStr logger (unlines $ map ("  "++) lib_paths_env)
      maybePutStrLn logger "Search directories (gcc):"
      maybePutStr logger (unlines $ map ("  "++) gcc_paths)

      libspecs
        <- mapM (locateLib interp hsc_env False lib_paths_env gcc_paths) minus_ls

      -- (d) Link .o files from the command-line
      classified_ld_inputs <- mapM (classifyLdInput logger platform)
                                [ f | FileOption _ f <- cmdline_ld_inputs ]

      -- (e) Link any MacOS frameworks
      let platform = targetPlatform dflags
      let (framework_paths, frameworks) =
            if platformUsesFrameworks platform
             then (frameworkPaths dflags, cmdlineFrameworks dflags)
              else ([],[])

      -- Finally do (c),(d),(e)
      let cmdline_lib_specs = catMaybes classified_ld_inputs
                           ++ libspecs
                           ++ map Framework frameworks
      if null cmdline_lib_specs
         then return pls
         else do
           -- Add directories to library search paths, this only has an effect
           -- on Windows. On Unix OSes this function is a NOP.
           let all_paths = let paths = takeDirectory (pgm_c dflags)
                                     : framework_paths
                                    ++ lib_paths_base
                                    ++ [ takeDirectory dll | DLLPath dll <- libspecs ]
                           in nub $ map normalise paths
           let lib_paths = nub $ lib_paths_base ++ gcc_paths
           all_paths_env <- addEnvPaths "LD_LIBRARY_PATH" all_paths
           pathCache <- mapM (addLibrarySearchPath interp) all_paths_env

           let merged_specs = mergeStaticObjects cmdline_lib_specs
           pls1 <- foldM (preloadLib interp hsc_env lib_paths framework_paths) pls
                         merged_specs

           maybePutStr logger "final link ... "
           ok <- resolveObjs interp

           -- DLLs are loaded, reset the search paths
           mapM_ (removeLibrarySearchPath interp) $ reverse pathCache

           if succeeded ok then maybePutStrLn logger "done"
           else throwGhcExceptionIO (ProgramError "linking extra libraries/objects failed")

           return pls1

-- | Merge runs of consecutive of 'Objects'. This allows for resolution of
-- cyclic symbol references when dynamically linking. Specifically, we link
-- together all of the static objects into a single shared object, avoiding
-- the issue we saw in #13786.
mergeStaticObjects :: [LibrarySpec] -> [LibrarySpec]
mergeStaticObjects specs = go [] specs
  where
    go :: [FilePath] -> [LibrarySpec] -> [LibrarySpec]
    go accum (Objects objs : rest) = go (objs ++ accum) rest
    go accum@(_:_) rest = Objects (reverse accum) : go [] rest
    go [] (spec:rest) = spec : go [] rest
    go [] [] = []

{- Note [preload packages]
   ~~~~~~~~~~~~~~~~~~~~~~~
Why do we need to preload packages from the command line?  This is an
explanation copied from #2437:

I tried to implement the suggestion from #3560, thinking it would be
easy, but there are two reasons we link in packages eagerly when they
are mentioned on the command line:

  * So that you can link in extra object files or libraries that
    depend on the packages. e.g. ghc -package foo -lbar where bar is a
    C library that depends on something in foo. So we could link in
    foo eagerly if and only if there are extra C libs or objects to
    link in, but....

  * Haskell code can depend on a C function exported by a package, and
    the normal dependency tracking that TH uses can't know about these
    dependencies. The test ghcilink004 relies on this, for example.

I conclude that we need two -package flags: one that says "this is a
package I want to make available", and one that says "this is a
package I want to link in eagerly". Would that be too complicated for
users?
-}

classifyLdInput :: Logger -> Platform -> FilePath -> IO (Maybe LibrarySpec)
classifyLdInput logger platform f
  | isObjectFilename platform f = return (Just (Objects [f]))
  | isDynLibFilename platform f = return (Just (DLLPath f))
  | otherwise          = do
        logMsg logger MCInfo noSrcSpan
            $ withPprStyle defaultUserStyle
            (text ("Warning: ignoring unrecognised input `" ++ f ++ "'"))
        return Nothing

preloadLib
  :: Interp
  -> HscEnv
  -> [String]
  -> [String]
  -> LoaderState
  -> LibrarySpec
  -> IO LoaderState
preloadLib interp hsc_env lib_paths framework_paths pls lib_spec = do
  maybePutStr logger ("Loading object " ++ showLS lib_spec ++ " ... ")
  case lib_spec of
    Objects static_ishs -> do
      (b, pls1) <- preload_statics lib_paths static_ishs
      maybePutStrLn logger (if b  then "done" else "not found")
      return pls1

    Archive static_ish -> do
      b <- preload_static_archive lib_paths static_ish
      maybePutStrLn logger (if b  then "done" else "not found")
      return pls

    DLL dll_unadorned -> do
      maybe_errstr <- loadDLL interp (platformSOName platform dll_unadorned)
      case maybe_errstr of
         Right _ -> maybePutStrLn logger "done"
         Left mm | platformOS platform /= OSDarwin ->
           preloadFailed mm lib_paths lib_spec
         Left mm | otherwise -> do
           -- As a backup, on Darwin, try to also load a .so file
           -- since (apparently) some things install that way - see
           -- ticket #8770.
           let libfile = ("lib" ++ dll_unadorned) <.> "so"
           err2 <- loadDLL interp libfile
           case err2 of
             Right _ -> maybePutStrLn logger "done"
             Left _  -> preloadFailed mm lib_paths lib_spec
      return pls

    DLLPath dll_path -> do
      do maybe_errstr <- loadDLL interp dll_path
         case maybe_errstr of
            Right _ -> maybePutStrLn logger "done"
            Left mm -> preloadFailed mm lib_paths lib_spec
         return pls

    Framework framework ->
      if platformUsesFrameworks (targetPlatform dflags)
      then do maybe_errstr <- loadFramework interp framework_paths framework
              case maybe_errstr of
                 Nothing -> maybePutStrLn logger "done"
                 Just mm -> preloadFailed mm framework_paths lib_spec
              return pls
      else throwGhcExceptionIO (ProgramError "preloadLib Framework")

  where
    dflags = hsc_dflags hsc_env
    logger = hsc_logger hsc_env

    platform = targetPlatform dflags

    preloadFailed :: String -> [String] -> LibrarySpec -> IO ()
    preloadFailed sys_errmsg paths spec
       = do maybePutStr logger "failed.\n"
            throwGhcExceptionIO $
              CmdLineError (
                    "user specified .o/.so/.DLL could not be loaded ("
                    ++ sys_errmsg ++ ")\nWhilst trying to load:  "
                    ++ showLS spec ++ "\nAdditional directories searched:"
                    ++ (if null paths then " (none)" else
                        intercalate "\n" (map ("   "++) paths)))

    -- Not interested in the paths in the static case.
    preload_statics _paths names
       = do b <- or <$> mapM doesFileExist names
            if not b then return (False, pls)
                     else if interpreterDynamic interp
                             then  do pls1 <- dynLoadObjs interp hsc_env pls names
                                      return (True, pls1)
                             else  do mapM_ (loadObj interp) names
                                      return (True, pls)

    preload_static_archive _paths name
       = do b <- doesFileExist name
            if not b then return False
                     else do if interpreterDynamic interp
                                 then throwGhcExceptionIO $
                                      CmdLineError dynamic_msg
                                 else loadArchive interp name
                             return True
      where
        dynamic_msg = unlines
          [ "User-specified static library could not be loaded ("
            ++ name ++ ")"
          , "Loading static libraries is not supported in this configuration."
          , "Try using a dynamic library instead."
          ]

initLinkDepsOpts :: HscEnv -> LinkDepsOpts
initLinkDepsOpts hsc_env = opts
  where
    opts = LinkDepsOpts
            { ldObjSuffix   = objectSuf dflags
            , ldForceDyn    = sTargetRTSLinkerOnlySupportsSharedLibs $ settings dflags
            , ldUnitEnv     = hsc_unit_env hsc_env
            , ldPprOpts     = initSDocContext dflags defaultUserStyle
            , ldFinderCache = hsc_FC hsc_env
            , ldFinderOpts  = initFinderOpts dflags
            , ldUseByteCode = gopt Opt_UseBytecodeRatherThanObjects dflags
            , ldMsgOpts     = initIfaceMessageOpts dflags
            , ldWays        = ways dflags
            , ldGetDependencies = get_reachable_nodes hsc_env
            , ldLoadByteCode
            }
    dflags = hsc_dflags hsc_env

    ldLoadByteCode mod = do
      _ <- initIfaceLoad hsc_env $
             loadInterface (text "get_reachable_nodes" <+> parens (ppr mod))
                 mod ImportBySystem
      EPS {eps_iface_bytecode} <- hscEPS hsc_env
      sequence (lookupModuleEnv eps_iface_bytecode mod)


get_reachable_nodes :: HscEnv -> [Module] -> IO ([Module], UniqDSet UnitId)
get_reachable_nodes hsc_env mods

  -- Fallback case if the ModuleGraph has not been initialised by the user.
  -- This can happen if is the user is loading plugins or doing something else very
  -- early in the compiler pipeline.
  | isEmptyMG (hsc_mod_graph hsc_env)
  = do
      mg <- downsweepInstalledModules hsc_env mods
      go mg

  | otherwise
  = go (hsc_mod_graph hsc_env)

  where
    unit_env = hsc_unit_env hsc_env
    mkModuleNk m = ModNodeKeyWithUid (GWIB (moduleName m) NotBoot) (moduleUnitId m)

    hmgModKey mg m
      | let k = NodeKey_Module (mkModuleNk m)
      , mgMember mg k = k
      | otherwise = NodeKey_ExternalUnit (moduleUnitId m)

    -- The main driver for getting dependencies, which calls the given
    -- functions to compute the reachable nodes.
    go :: ModuleGraph -> IO ([Module], UniqDSet UnitId)
    go mg = do
        let mod_keys = map (hmgModKey mg) mods
            all_reachable = mod_keys ++ map mkNodeKey (mgReachableLoop mg mod_keys)
        (mods_s, pkgs_s) <- partitionEithers <$> mapMaybeM get_mod_info all_reachable
        return (mods_s, mkUniqDSet pkgs_s)

    get_mod_info :: NodeKey -> IO (Maybe (Either Module UnitId))
    get_mod_info (NodeKey_Module m@(ModNodeKeyWithUid gwib uid)) =
      lookupHug (ue_home_unit_graph unit_env) uid (gwib_mod gwib) >>= \case
        Just hmi -> return $ Just (Left  (mi_module (hm_iface hmi)))
        Nothing -> return (Just (Left (mnkToModule m)))
    get_mod_info (NodeKey_ExternalUnit uid) = return (Just (Right uid))
    get_mod_info _ = return Nothing


{- **********************************************************************

              Loading a Decls statement

  ********************************************************************* -}

loadDecls :: Interp -> HscEnv -> SrcSpan -> Linkable -> IO ([(Name, ForeignHValue)], [Linkable], PkgsLoaded)
loadDecls interp hsc_env span linkable = do
    -- Initialise the linker (if it's not been done already)
    initLoaderState interp hsc_env

    -- Take lock for the actual work.
    modifyLoaderState interp $ \pls0 -> do
      -- Link the foreign objects first; BCOs in linkable are ignored here.
      (pls1, objs_ok) <- loadObjects interp hsc_env pls0 [linkable]
      when (failed objs_ok) $ throwGhcExceptionIO $ ProgramError "loadDecls: failed to load foreign objects"

      -- Link the packages and modules required
      (pls, ok, links_needed, units_needed) <- loadDependencies interp hsc_env pls1 span needed_mods
      if failed ok
        then throwGhcExceptionIO (ProgramError "")
        else do
          -- Link the expression itself
          let le  = linker_env pls
          le2_itbl_env <- linkITbls interp (itbl_env le) (concat $ map bc_itbls cbcs)
          le2_addr_env <- foldlM (\env cbc -> allocateTopStrings interp (bc_strs cbc) env) (addr_env le) cbcs
          le2_breakarray_env <-
            allocateBreakArrays
              interp
              (catMaybes $ map bc_breaks cbcs)
              (breakarray_env le)
          le2_ccs_env <-
            allocateCCS
              interp
              (catMaybes $ map bc_breaks cbcs)
              (ccs_env le)
          let le2 = le { itbl_env = le2_itbl_env
                       , addr_env = le2_addr_env
                       , breakarray_env = le2_breakarray_env
                       , ccs_env = le2_ccs_env }

          -- Link the necessary packages and linkables
          new_bindings <- linkSomeBCOs interp (pkgs_loaded pls) le2 cbcs
          nms_fhvs <- makeForeignNamedHValueRefs interp new_bindings
          let ce2  = extendClosureEnv (closure_env le2) nms_fhvs
              !pls2 = pls { linker_env = le2 { closure_env = ce2 } }
          return (pls2, (nms_fhvs, links_needed, units_needed))
  where
    cbcs = linkableBCOs linkable

    free_names = uniqDSetToList $
      foldl'
        (\acc cbc -> foldl' (\acc' bco -> bcoFreeNames bco `unionUniqDSets` acc') acc (bc_bcos cbc))
        emptyUniqDSet cbcs

    needed_mods :: [Module]
    needed_mods = [ nameModule n | n <- free_names,
                    isExternalName n,       -- Names from other modules
                    not (isWiredInName n)   -- Exclude wired-in names
                  ]                         -- (see note below)
    -- Exclude wired-in names because we may not have read
    -- their interface files, so getLinkDeps will fail
    -- All wired-in names are in the base package, which we link
    -- by default, so we can safely ignore them here.

{- **********************************************************************

              Loading a single module

  ********************************************************************* -}

loadModule :: Interp -> HscEnv -> Module -> IO ()
loadModule interp hsc_env mod = do
  initLoaderState interp hsc_env
  modifyLoaderState_ interp $ \pls -> do
    (pls', ok, _, _) <- loadDependencies interp hsc_env pls noSrcSpan [mod]
    if failed ok
      then throwGhcExceptionIO (ProgramError "could not load module")
      else return pls'

{- **********************************************************************

                Link some linkables
        The linkables may consist of a mixture of
        byte-code modules and object modules

  ********************************************************************* -}

loadModuleLinkables :: Interp -> HscEnv -> LoaderState -> [Linkable] -> IO (LoaderState, SuccessFlag)
loadModuleLinkables interp hsc_env pls linkables
  = mask_ $ do  -- don't want to be interrupted by ^C in here

        debugTraceMsg (hsc_logger hsc_env) 3 $
          hang (text "Loading module linkables") 2 $ vcat [
            hang (text "Objects:") 2 (vcat (ppr <$> objs)),
            hang (text "Bytecode:") 2 (vcat (ppr <$> bcos))
          ]

                -- Load objects first; they can't depend on BCOs
        (pls1, ok_flag) <- loadObjects interp hsc_env pls objs

        if failed ok_flag then
                return (pls1, Failed)
          else do
                pls2 <- dynLinkBCOs interp pls1 bcos
                return (pls2, Succeeded)
  where
    (objs, bcos) = partitionLinkables linkables


linkableInSet :: Linkable -> LinkableSet -> Bool
linkableInSet l objs_loaded =
  case lookupModuleEnv objs_loaded (linkableModule l) of
        Nothing -> False
        Just m  -> linkableTime l == linkableTime m


{- **********************************************************************

                The object-code linker

  ********************************************************************* -}

-- | Load the object files and link them
--
-- If the interpreter uses dynamic-linking, build a shared library and load it.
-- Otherwise, use the RTS linker.
loadObjects
  :: Interp
  -> HscEnv
  -> LoaderState
  -> [Linkable]
  -> IO (LoaderState, SuccessFlag)
loadObjects interp hsc_env pls objs = do
        let (objs_loaded', new_objs) = rmDupLinkables (objs_loaded pls) objs
            pls1                     = pls { objs_loaded = objs_loaded' }
            wanted_objs              = concatMap linkableFiles new_objs

        if interpreterDynamic interp
            then do pls2 <- dynLoadObjs interp hsc_env pls1 wanted_objs
                    return (pls2, Succeeded)
            else do mapM_ (loadObj interp) wanted_objs

                    -- Link them all together
                    ok <- resolveObjs interp

                    -- If resolving failed, unload all our
                    -- object modules and carry on
                    if succeeded ok then
                            return (pls1, Succeeded)
                      else do
                            pls2 <- unload_wkr interp [] pls1
                            return (pls2, Failed)


-- | Create a shared library containing the given object files and load it.
dynLoadObjs :: Interp -> HscEnv -> LoaderState -> [FilePath] -> IO LoaderState
dynLoadObjs _      _       pls                           []   = return pls
dynLoadObjs interp hsc_env pls@LoaderState{..} objs = do
    let unit_env = hsc_unit_env hsc_env
    let dflags   = hsc_dflags hsc_env
    let logger   = hsc_logger hsc_env
    let tmpfs    = hsc_tmpfs hsc_env
    let platform = ue_platform unit_env
    let minus_ls = [ lib | Option ('-':'l':lib) <- ldInputs dflags ]
    let minus_big_ls = [ lib | Option ('-':'L':lib) <- ldInputs dflags ]
    (soFile, libPath , libName) <-
      newTempLibName logger tmpfs (tmpDir dflags) TFL_CurrentModule (platformSOExt platform)
    let
        dflags2 = dflags {
                      -- We don't want the original ldInputs in
                      -- (they're already linked in), but we do want
                      -- to link against previous dynLoadObjs
                      -- libraries if there were any, so that the linker
                      -- can resolve dependencies when it loads this
                      -- library.
                      ldInputs =
                           concatMap (\l -> [ Option ("-l" ++ l) ])
                                     (nub $ snd <$> temp_sos)
                        ++ concatMap (\lp -> Option ("-L" ++ lp)
                                          : if useXLinkerRPath dflags (platformOS platform)
                                            then [ Option "-Xlinker"
                                                 , Option "-rpath"
                                                 , Option "-Xlinker"
                                                 , Option lp ]
                                            else [])
                                     (nub $ fst <$> temp_sos)
                        ++ concatMap
                             (\lp -> Option ("-L" ++ lp)
                                  : if useXLinkerRPath dflags (platformOS platform)
                                    then [ Option "-Xlinker"
                                         , Option "-rpath"
                                         , Option "-Xlinker"
                                         , Option lp ]
                                    else [])
                             minus_big_ls
                        -- See Note [-Xlinker -rpath vs -Wl,-rpath]
                        ++ map (\l -> Option ("-l" ++ l)) minus_ls,


                      -- Add -l options and -L options from dflags.
                      --
                      -- When running TH for a non-dynamic way, we still
                      -- need to make -l flags to link against the dynamic
                      -- libraries, so we need to add WayDyn to ways.
                      --
                      -- Likewise if loading if the profiled way then need to
                      -- add WayProf.
                      targetWays_ = let ws = Set.singleton WayDyn
                                     in if interpreterProfiled interp
                                        then addWay WayProf ws
                                        else ws,
                      outputFile_ = Just soFile
                  }
    -- link all "loaded packages" so symbols in those can be resolved
    -- Note: We are loading packages with local scope, so to see the
    -- symbols in this link we must link all loaded packages again.
    linkDynLib logger tmpfs dflags2 unit_env objs (loaded_pkg_uid <$> eltsUDFM pkgs_loaded)

    -- if we got this far, extend the lifetime of the library file
    changeTempFilesLifetime tmpfs TFL_GhcSession [soFile]
    m <- loadDLL interp soFile
    case m of
      Right _ -> return $! pls { temp_sos = (libPath, libName) : temp_sos }
      Left err -> linkFail msg (text err)
  where
    msg = "GHC.Linker.Loader.dynLoadObjs: Loading temp shared object failed"

rmDupLinkables :: LinkableSet    -- Already loaded
               -> [Linkable]    -- New linkables
               -> (LinkableSet,  -- New loaded set (including new ones)
                   [Linkable])  -- New linkables (excluding dups)
rmDupLinkables already ls
  = go already [] ls
  where
    go already extras [] = (already, extras)
    go already extras (l:ls)
        | linkableInSet l already = go already     extras     ls
        | otherwise               = go (extendModuleEnv already (linkableModule l) l) (l:extras) ls

{- **********************************************************************

                The byte-code linker

  ********************************************************************* -}


dynLinkBCOs :: Interp -> LoaderState -> [Linkable] -> IO LoaderState
dynLinkBCOs interp pls bcos = do

        let (bcos_loaded', new_bcos) = rmDupLinkables (bcos_loaded pls) bcos
            pls1                     = pls { bcos_loaded = bcos_loaded' }

            parts :: [LinkablePart]
            parts = concatMap (NE.toList . linkableParts) new_bcos

            cbcs :: [CompiledByteCode]
            cbcs = concatMap linkablePartAllBCOs parts


            le1 = linker_env pls
        ie2 <- linkITbls interp (itbl_env le1) (concatMap bc_itbls cbcs)
        ae2 <- foldlM (\env cbc -> allocateTopStrings interp (bc_strs cbc) env) (addr_env le1) cbcs
        be2 <-
          allocateBreakArrays
            interp
            (catMaybes $ map bc_breaks cbcs)
            (breakarray_env le1)
        ce2 <- allocateCCS interp (catMaybes $ map bc_breaks cbcs) (ccs_env le1)
        let le2 = le1 { itbl_env = ie2, addr_env = ae2, breakarray_env = be2, ccs_env = ce2 }

        names_and_refs <- linkSomeBCOs interp (pkgs_loaded pls) le2 cbcs

        -- We only want to add the external ones to the ClosureEnv
        let (to_add, to_drop) = partition (isExternalName.fst) names_and_refs

        -- Immediately release any HValueRefs we're not going to add
        freeHValueRefs interp (map snd to_drop)
        -- Wrap finalizers on the ones we want to keep
        new_binds <- makeForeignNamedHValueRefs interp to_add

        let ce2 = extendClosureEnv (closure_env le2) new_binds
        return $! pls1 { linker_env = le2 { closure_env = ce2 } }

-- Link a bunch of BCOs and return references to their values
linkSomeBCOs :: Interp
             -> PkgsLoaded
             -> LinkerEnv
             -> [CompiledByteCode]
             -> IO [(Name,HValueRef)]
                        -- The returned HValueRefs are associated 1-1 with
                        -- the incoming unlinked BCOs.  Each gives the
                        -- value of the corresponding unlinked BCO

linkSomeBCOs interp pkgs_loaded le mods = foldr fun do_link mods []
 where
  fun CompiledByteCode{..} inner accum =
    inner (Foldable.toList bc_bcos : accum)

  do_link [] = return []
  do_link mods = do
    let flat = [ bco | bcos <- mods, bco <- bcos ]
        names = map unlinkedBCOName flat
        bco_ix = mkNameEnv (zip names [0..])
    resolved <- sequence [ linkBCO interp pkgs_loaded le bco_ix bco | bco <- flat ]
    hvrefs <- createBCOs interp resolved
    return (zip names hvrefs)

-- | Useful to apply to the result of 'linkSomeBCOs'
makeForeignNamedHValueRefs
  :: Interp -> [(Name,HValueRef)] -> IO [(Name,ForeignHValue)]
makeForeignNamedHValueRefs interp bindings =
  mapM (\(n, hvref) -> (n,) <$> mkFinalizedHValue interp hvref) bindings

linkITbls :: Interp -> ItblEnv -> [(Name, ConInfoTable)] -> IO ItblEnv
linkITbls interp = foldlM $ \env (nm, itbl) -> do
  r <- interpCmd interp $ MkConInfoTable itbl
  evaluate $ extendNameEnv env nm (nm, ItblPtr r)

{- **********************************************************************

                Unload some object modules

  ********************************************************************* -}

-- ---------------------------------------------------------------------------
-- | Unloading old objects ready for a new compilation sweep.
--
-- The compilation manager provides us with a list of linkables that it
-- considers \"stable\", i.e. won't be recompiled this time around.  For
-- each of the modules current linked in memory,
--
--   * if the linkable is stable (and it's the same one -- the user may have
--     recompiled the module on the side), we keep it,
--
--   * otherwise, we unload it.
--
--   * we also implicitly unload all temporary bindings at this point.
--
unload
  :: Interp
  -> HscEnv
  -> [Linkable] -- ^ The linkables to *keep*.
  -> IO ()
unload interp hsc_env linkables
  = mask_ $ do -- mask, so we're safe from Ctrl-C in here

        -- Initialise the linker (if it's not been done already)
        initLoaderState interp hsc_env

        new_pls
            <- modifyLoaderState interp $ \pls -> do
                 pls1 <- unload_wkr interp linkables pls
                 return (pls1, pls1)

        let logger = hsc_logger hsc_env
        debugTraceMsg logger 3 $
          text "unload: retaining objs" <+> ppr (moduleEnvElts $ objs_loaded new_pls)
        debugTraceMsg logger 3 $
          text "unload: retaining bcos" <+> ppr (moduleEnvElts $ bcos_loaded new_pls)
        return ()

unload_wkr
  :: Interp
  -> [Linkable]                -- stable linkables
  -> LoaderState
  -> IO LoaderState
-- Does the core unload business
-- (the wrapper blocks exceptions and deals with the LS get and put)

unload_wkr interp keep_linkables pls@LoaderState{..}  = do
  -- NB. careful strictness here to avoid keeping the old LS when
  -- we're unloading some code.  -fghci-leak-check with the tests in
  -- testsuite/ghci can detect space leaks here.

  let (objs_to_keep', bcos_to_keep') = partition linkableIsNativeCodeOnly keep_linkables
      objs_to_keep = mkLinkableSet objs_to_keep'
      bcos_to_keep = mkLinkableSet bcos_to_keep'

      discard keep l = not (linkableInSet l keep)

      (objs_to_unload, remaining_objs_loaded) =
         partitionModuleEnv (discard objs_to_keep) objs_loaded
      (bcos_to_unload, remaining_bcos_loaded) =
         partitionModuleEnv (discard bcos_to_keep) bcos_loaded

      linkables_to_unload = moduleEnvElts objs_to_unload ++ moduleEnvElts bcos_to_unload

  mapM_ unloadObjs linkables_to_unload

  -- If we unloaded any object files at all, we need to purge the cache
  -- of lookupSymbol results.
  when (not (null (filter (not . null . linkableObjs) linkables_to_unload))) $
    purgeLookupSymbolCache interp

  let -- Note that we want to remove all *local*
      -- (i.e. non-isExternal) names too (these are the
      -- temporary bindings from the command line).
      keep_name :: Name -> Bool
      keep_name n = isExternalName n &&
                    nameModule n `elemModuleEnv` remaining_bcos_loaded

      !new_pls = pls { linker_env = filterLinkerEnv keep_name linker_env,
                       bcos_loaded = remaining_bcos_loaded,
                       objs_loaded = remaining_objs_loaded }

  return new_pls
  where
    unloadObjs :: Linkable -> IO ()
    unloadObjs lnk
      | interpreterDynamic interp = return ()
        -- We don't do any cleanup when linking objects with the
        -- dynamic linker.  Doing so introduces extra complexity for
        -- not much benefit.

      | otherwise
      = mapM_ (unloadObj interp) (linkableObjs lnk)
                -- The components of a BCO linkable may contain
                -- dot-o files (generated from C stubs).
                --
                -- But the BCO parts can be unlinked just by
                -- letting go of them (plus of course depopulating
                -- the symbol table which is done in the main body)

showLS :: LibrarySpec -> String
showLS (Objects nms)  = "(static) [" ++ intercalate ", " nms ++ "]"
showLS (Archive nm)   = "(static archive) " ++ nm
showLS (DLL nm)       = "(dynamic) " ++ nm
showLS (DLLPath nm)   = "(dynamic) " ++ nm
showLS (Framework nm) = "(framework) " ++ nm

-- | Load exactly the specified packages, and their dependents (unless of
-- course they are already loaded).  The dependents are loaded
-- automatically, and it doesn't matter what order you specify the input
-- packages.
--
loadPackages :: Interp -> HscEnv -> [UnitId] -> IO ()
-- NOTE: in fact, since each module tracks all the packages it depends on,
--       we don't really need to use the package-config dependencies.
--
-- However we do need the package-config stuff (to find aux libs etc),
-- and following them lets us load libraries in the right order, which
-- perhaps makes the error message a bit more localised if we get a link
-- failure.  So the dependency walking code is still here.

loadPackages interp hsc_env new_pkgs = do
  -- It's probably not safe to try to load packages concurrently, so we take
  -- a lock.
  initLoaderState interp hsc_env
  modifyLoaderState_ interp $ \pls ->
    loadPackages' interp hsc_env new_pkgs pls

loadPackages' :: Interp -> HscEnv -> [UnitId] -> LoaderState -> IO LoaderState
loadPackages' interp hsc_env new_pks pls = do
    pkgs' <- link (pkgs_loaded pls) new_pks
    return $! pls { pkgs_loaded = pkgs'
                  }
  where
     link :: PkgsLoaded -> [UnitId] -> IO PkgsLoaded
     link pkgs new_pkgs =
         foldM link_one pkgs new_pkgs

     link_one pkgs new_pkg
        | new_pkg `elemUDFM` pkgs   -- Already linked
        = return pkgs

        | Just pkg_cfg <- lookupUnitId (hsc_units hsc_env) new_pkg
        = do { let deps = unitDepends pkg_cfg
               -- Link dependents first
             ; pkgs' <- link pkgs deps
                -- Now link the package itself
             ; (hs_cls, extra_cls, loaded_dlls) <- loadPackage interp hsc_env pkg_cfg
             ; let trans_deps = unionManyUniqDSets [ addOneToUniqDSet (loaded_pkg_trans_deps loaded_pkg_info) dep_pkg
                                                   | dep_pkg <- deps
                                                   , Just loaded_pkg_info <- pure (lookupUDFM pkgs' dep_pkg)
                                                   ]
             ; return (addToUDFM pkgs' new_pkg (LoadedPkgInfo new_pkg hs_cls extra_cls loaded_dlls trans_deps)) }

        | otherwise
        = throwGhcExceptionIO (CmdLineError ("unknown package: " ++ unpackFS (unitIdFS new_pkg)))


loadPackage :: Interp -> HscEnv -> UnitInfo -> IO ([LibrarySpec], [LibrarySpec], [RemotePtr LoadedDLL])
loadPackage interp hsc_env pkg
   = do
        let dflags    = hsc_dflags hsc_env
        let logger    = hsc_logger hsc_env
            platform  = targetPlatform dflags
            is_dyn    = interpreterDynamic interp
            dirs | is_dyn    = map ST.unpack $ Packages.unitLibraryDynDirs pkg
                 | otherwise = map ST.unpack $ Packages.unitLibraryDirs pkg

        let hs_libs   = map ST.unpack $ Packages.unitLibraries pkg
            -- The FFI GHCi import lib isn't needed as
            -- GHC.Linker.Loader + rts/Linker.c link the
            -- interpreted references to FFI to the compiled FFI.
            -- We therefore filter it out so that we don't get
            -- duplicate symbol errors.
            hs_libs'  =  filter ("HSffi" /=) hs_libs

        -- Because of slight differences between the GHC dynamic linker and
        -- the native system linker some packages have to link with a
        -- different list of libraries when using GHCi. Examples include: libs
        -- that are actually gnu ld scripts, and the possibility that the .a
        -- libs do not exactly match the .so/.dll equivalents. So if the
        -- package file provides an "extra-ghci-libraries" field then we use
        -- that instead of the "extra-libraries" field.
            extdeplibs = map ST.unpack (if null (Packages.unitExtDepLibsGhc pkg)
                                      then Packages.unitExtDepLibsSys pkg
                                      else Packages.unitExtDepLibsGhc pkg)
            linkerlibs = [ lib | '-':'l':lib <- (map ST.unpack $ Packages.unitLinkerOptions pkg) ]
            extra_libs = extdeplibs ++ linkerlibs

        -- See Note [Fork/Exec Windows]
        gcc_paths <- getGCCPaths logger dflags (platformOS platform)
        dirs_env <- addEnvPaths "LIBRARY_PATH" dirs

        hs_classifieds
           <- mapM (locateLib interp hsc_env True  dirs_env gcc_paths) hs_libs'
        extra_classifieds
           <- mapM (locateLib interp hsc_env False dirs_env gcc_paths) extra_libs
        let classifieds = hs_classifieds ++ extra_classifieds

        -- Complication: all the .so's must be loaded before any of the .o's.
        let known_hs_dlls    = [ dll | DLLPath dll <- hs_classifieds ]
            known_extra_dlls = [ dll | DLLPath dll <- extra_classifieds ]
            known_dlls       = known_hs_dlls ++ known_extra_dlls
#if defined(CAN_LOAD_DLL)
            dlls       = [ dll  | DLL dll        <- classifieds ]
#endif
            objs       = [ obj  | Objects objs    <- classifieds
                                , obj <- objs ]
            archs      = [ arch | Archive arch   <- classifieds ]

        -- Add directories to library search paths
        let dll_paths  = map takeDirectory known_dlls
            all_paths  = nub $ map normalise $ dll_paths ++ dirs
        all_paths_env <- addEnvPaths "LD_LIBRARY_PATH" all_paths
        pathCache <- mapM (addLibrarySearchPath interp) all_paths_env

        maybePutSDoc logger
            (text "Loading unit " <> pprUnitInfoForUser pkg <> text " ... ")

#if defined(CAN_LOAD_DLL)
        loadFrameworks interp platform pkg
        -- See Note [Crash early load_dyn and locateLib]
        -- Crash early if can't load any of `known_dlls`
        mapM_ (load_dyn interp hsc_env True) known_extra_dlls
        loaded_dlls <- mapMaybeM (load_dyn interp hsc_env True) known_hs_dlls
        -- For remaining `dlls` crash early only when there is surely
        -- no package's DLL around ... (not is_dyn)
        mapM_ (load_dyn interp hsc_env (not is_dyn) . platformSOName platform) dlls
#else
        let loaded_dlls = []
#endif
        -- After loading all the DLLs, we can load the static objects.
        -- Ordering isn't important here, because we do one final link
        -- step to resolve everything.
        mapM_ (loadObj interp) objs
        mapM_ (loadArchive interp) archs

        maybePutStr logger "linking ... "
        ok <- resolveObjs interp

        -- DLLs are loaded, reset the search paths
        -- Import libraries will be loaded via loadArchive so only
        -- reset the DLL search path after all archives are loaded
        -- as well.
        mapM_ (removeLibrarySearchPath interp) $ reverse pathCache

        if succeeded ok
           then do
             maybePutStrLn logger "done."
             return (hs_classifieds, extra_classifieds, loaded_dlls)
           else let errmsg = text "unable to load unit `"
                             <> pprUnitInfoForUser pkg <> text "'"
                 in throwGhcExceptionIO (InstallationError (showSDoc dflags errmsg))

{-
Note [Crash early load_dyn and locateLib]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If a package is "normal" (exposes it's code from more than zero Haskell
modules, unlike e.g. that in ghcilink004) and is built "dyn" way, then
it has it's code compiled and linked into the DLL, which GHCi linker picks
when loading the package's code (see the big comment in the beginning of
`locateLib`).

When loading DLLs, GHCi linker simply calls the system's `dlopen` or
`LoadLibrary` APIs. This is quite different from the case when GHCi linker
loads an object file or static library. When loading an object file or static
library GHCi linker parses them and resolves all symbols "manually".
These object file or static library may reference some external symbols
defined in some external DLLs. And GHCi should know which these
external DLLs are.

But when GHCi loads a DLL, it's the *system* linker who manages all
the necessary dependencies, and it is able to load this DLL not having
any extra info. Thus we don't *have to* crash in this case even if we
are unable to load any supposed dependencies explicitly.

Suppose during GHCi session a client of the package wants to
`foreign import` a symbol which isn't exposed by the package DLL, but
is exposed by such an external (dependency) DLL.
If the DLL isn't *explicitly* loaded because `load_dyn` failed to do
this, then the client code eventually crashes because the GHCi linker
isn't able to locate this symbol (GHCi linker maintains a list of
explicitly loaded DLLs it looks into when trying to find a symbol).

This is why we still should try to load all the dependency DLLs
even though we know that the system linker loads them implicitly when
loading the package DLL.

Why we still keep the `crash_early` opportunity then not allowing such
a permissive behaviour for any DLLs? Well, we, perhaps, improve a user
experience in some cases slightly.

But if it happens there exist other corner cases where our current
usage of `crash_early` flag is overly restrictive, we may lift the
restriction very easily.
-}

#if defined(CAN_LOAD_DLL)
-- we have already searched the filesystem; the strings passed to load_dyn
-- can be passed directly to loadDLL.  They are either fully-qualified
-- ("/usr/lib/libfoo.so"), or unqualified ("libfoo.so").  In the latter case,
-- loadDLL is going to search the system paths to find the library.
load_dyn :: Interp -> HscEnv -> Bool -> FilePath -> IO (Maybe (RemotePtr LoadedDLL))
load_dyn interp hsc_env crash_early dll = do
  r <- loadDLL interp dll
  case r of
    Right loaded_dll -> pure (Just loaded_dll)
    Left err ->
      if crash_early
        then cmdLineErrorIO err
        else do
          when (diag_wopt Opt_WarnMissedExtraSharedLib diag_opts)
            $ logMsg logger
                (mkMCDiagnostic diag_opts (WarningWithFlag Opt_WarnMissedExtraSharedLib) Nothing)
                  noSrcSpan $ withPprStyle defaultUserStyle (note err)
          pure Nothing
  where
    diag_opts = initDiagOpts (hsc_dflags hsc_env)
    logger = hsc_logger hsc_env
    note err = vcat $ map text
      [ err
      , "It's OK if you don't want to use symbols from it directly."
      , "(the package DLL is loaded by the system linker"
      , " which manages dependencies by itself)." ]

loadFrameworks :: Interp -> Platform -> UnitInfo -> IO ()
loadFrameworks interp platform pkg
    = when (platformUsesFrameworks platform) $ mapM_ load frameworks
  where
    fw_dirs    = map ST.unpack $ Packages.unitExtDepFrameworkDirs pkg
    frameworks = map ST.unpack $ Packages.unitExtDepFrameworks pkg

    load fw = do  r <- loadFramework interp fw_dirs fw
                  case r of
                    Nothing  -> return ()
                    Just err -> cmdLineErrorIO ("can't load framework: "
                                                ++ fw ++ " (" ++ err ++ ")" )
#endif

-- Try to find an object file for a given library in the given paths.
-- If it isn't present, we assume that addDLL in the RTS can find it,
-- which generally means that it should be a dynamic library in the
-- standard system search path.
-- For GHCi we tend to prefer dynamic libraries over static ones as
-- they are easier to load and manage, have less overhead.
locateLib
  :: Interp
  -> HscEnv
  -> Bool
  -> [FilePath]
  -> [FilePath]
  -> String
  -> IO LibrarySpec
locateLib interp hsc_env is_hs lib_dirs gcc_dirs lib0
  | not is_hs
    -- For non-Haskell libraries (e.g. gmp, iconv):
    --   first look in library-dirs for a dynamic library (on User paths only)
    --   (libfoo.so)
    --   then  try looking for import libraries on Windows (on User paths only)
    --   (.dll.a, .lib)
    --   first look in library-dirs for a dynamic library (on GCC paths only)
    --   (libfoo.so)
    --   then  check for system dynamic libraries (e.g. kernel32.dll on windows)
    --   then  try looking for import libraries on Windows (on GCC paths only)
    --   (.dll.a, .lib)
    --   then  look in library-dirs for a static library (libfoo.a)
    --   then look in library-dirs and inplace GCC for a dynamic library (libfoo.so)
    --   then  try looking for import libraries on Windows (.dll.a, .lib)
    --   then  look in library-dirs and inplace GCC for a static library (libfoo.a)
    --   then  try "gcc --print-file-name" to search gcc's search path
    --       for a dynamic library (#5289)
    --   otherwise, assume loadDLL can find it
    --
    --   The logic is a bit complicated, but the rationale behind it is that
    --   loading a shared library for us is O(1) while loading an archive is
    --   O(n). Loading an import library is also O(n) so in general we prefer
    --   shared libraries because they are simpler and faster.
    --
  =
#if defined(CAN_LOAD_DLL)
    findDll   user `orElse`
#endif
    tryImpLib user `orElse`
#if defined(CAN_LOAD_DLL)
    findDll   gcc  `orElse`
    findSysDll     `orElse`
#endif
    tryImpLib gcc  `orElse`
    findArchive    `orElse`
    tryGcc         `orElse`
    assumeDll

  | loading_dynamic_hs_libs -- search for .so libraries first.
  = findHSDll     `orElse`
    findDynObject `orElse`
    assumeDll

  | otherwise
    -- use HSfoo.{o,p_o} if it exists, otherwise fallback to libHSfoo{,_p}.a
  = findObject  `orElse`
    findArchive `orElse`
    assumeDll

   where
     dflags = hsc_dflags hsc_env
     logger = hsc_logger hsc_env
     diag_opts = initDiagOpts dflags
     dirs   = lib_dirs ++ gcc_dirs
     gcc    = False
     user   = True

     -- Emulate ld's behavior of treating $LIB in `-l:$LIB` as a literal file
     -- name
     (lib, verbatim) = case lib0 of
       ':' : rest -> (rest, True)
       other      -> (other, False)

     obj_file
       | is_hs && loading_profiled_hs_libs = lib <.> "p_o"
       | otherwise = lib <.> "o"
     dyn_obj_file = lib <.> "dyn_o"
     arch_files
       | verbatim = [lib]
       | otherwise = [ "lib" ++ lib ++ lib_tag <.> "a"
                     , lib <.> "a" -- native code has no lib_tag
                     , "lib" ++ lib
                     , lib
                     ]
     lib_tag = if is_hs && loading_profiled_hs_libs then "_p" else ""

     loading_profiled_hs_libs = interpreterProfiled interp
     loading_dynamic_hs_libs  = interpreterDynamic  interp

     import_libs
       | verbatim = [lib]
       | otherwise = [ lib <.> "lib"
                     , "lib" ++ lib <.> "lib"
                     , "lib" ++ lib <.> "dll.a"
                     , lib <.> "dll.a"
                     ]

     hs_dyn_lib_name = lib ++ lib_tag ++ dynLibSuffix (ghcNameVersion dflags)
     hs_dyn_lib_file = platformHsSOName platform hs_dyn_lib_name

#if defined(CAN_LOAD_DLL)
     so_name     = platformSOName platform lib
     lib_so_name = "lib" ++ so_name
     dyn_lib_file
       | verbatim && any (`isExtensionOf` lib) [".so", ".dylib", ".dll"]
       = lib

       | ArchX86_64 <- arch
       , OSSolaris2 <- os
       = "64" </> so_name

       | otherwise
        = so_name
#endif

     findObject    = liftM (fmap $ Objects . (:[]))  $ findFile dirs obj_file
     findDynObject = liftM (fmap $ Objects . (:[]))  $ findFile dirs dyn_obj_file
     findArchive   = let local name = liftM (fmap Archive) $ findFile dirs name
                     in  apply (map local arch_files)
     findHSDll     = liftM (fmap DLLPath) $ findFile dirs hs_dyn_lib_file
#if defined(CAN_LOAD_DLL)
     findDll    re = let dirs' = if re == user then lib_dirs else gcc_dirs
                     in liftM (fmap DLLPath) $ findFile dirs' dyn_lib_file
     findSysDll    = fmap (fmap $ DLL . dropExtension . takeFileName) $
                        findSystemLibrary interp so_name
#endif
     tryGcc        = let search   = searchForLibUsingGcc logger dflags
#if defined(CAN_LOAD_DLL)
                         dllpath  = liftM (fmap DLLPath)
                         short    = dllpath $ search so_name lib_dirs
                         full     = dllpath $ search lib_so_name lib_dirs
                         dlls     = [short, full]
#endif
                         gcc name = liftM (fmap Archive) $ search name lib_dirs
                         files    = import_libs ++ arch_files
                         archives = map gcc files
                     in apply $
#if defined(CAN_LOAD_DLL)
                          dlls ++
#endif
                          archives
     tryImpLib re = case os of
                       OSMinGW32 ->
                        let dirs' = if re == user then lib_dirs else gcc_dirs
                            implib name = liftM (fmap Archive) $
                                            findFile dirs' name
                        in apply (map implib import_libs)
                       _         -> return Nothing

     -- TH Makes use of the interpreter so this failure is not obvious.
     -- So we are nice and warn/inform users why we fail before we do.
     -- But only for haskell libraries, as C libraries don't have a
     -- profiling/non-profiling distinction to begin with.
     assumeDll
      | is_hs
      , not loading_dynamic_hs_libs
      , interpreterProfiled interp
      = do
          let diag = mkMCDiagnostic diag_opts WarningWithoutFlag Nothing
          logMsg logger diag noSrcSpan $ withPprStyle defaultErrStyle $
            text "Interpreter failed to load profiled static library" <+> text lib <> char '.' $$
              text " \tTrying dynamic library instead. If this fails try to rebuild" <+>
              text "libraries with profiling support."
          return (DLL lib)
      | otherwise = return (DLL lib)
     infixr `orElse`
     f `orElse` g = f >>= maybe g return

     apply :: [IO (Maybe a)] -> IO (Maybe a)
     apply []     = return Nothing
     apply (x:xs) = do x' <- x
                       if isJust x'
                          then return x'
                          else apply xs

     platform = targetPlatform dflags
#if defined(CAN_LOAD_DLL)
     arch = platformArch platform
#endif
     os = platformOS platform

searchForLibUsingGcc :: Logger -> DynFlags -> String -> [FilePath] -> IO (Maybe FilePath)
searchForLibUsingGcc logger dflags so dirs = do
   -- GCC does not seem to extend the library search path (using -L) when using
   -- --print-file-name. So instead pass it a new base location.
   str <- askLd logger dflags (map (FileOption "-B") dirs
                          ++ [Option "--print-file-name", Option so])
   let file = case lines str of
                []  -> ""
                l:_ -> l
   if (file == so)
      then return Nothing
      else do b <- doesFileExist file -- file could be a folder (see #16063)
              return (if b then Just file else Nothing)

-- | Retrieve the list of search directory GCC and the System use to find
--   libraries and components. See Note [Fork/Exec Windows].
getGCCPaths :: Logger -> DynFlags -> OS -> IO [FilePath]
getGCCPaths logger dflags os
  | os == OSMinGW32 || platformArch (targetPlatform dflags) == ArchWasm32 =
        do gcc_dirs <- getGccSearchDirectory logger dflags "libraries"
           sys_dirs <- getSystemDirectories
           return $ nub $ gcc_dirs ++ sys_dirs
  | otherwise = return []

-- | Cache for the GCC search directories as this can't easily change
--   during an invocation of GHC. (Maybe with some env. variable but we'll)
--   deal with that highly unlikely scenario then.
{-# NOINLINE gccSearchDirCache #-}
gccSearchDirCache :: IORef [(String, [String])]
gccSearchDirCache = unsafePerformIO $ newIORef []

-- Note [Fork/Exec Windows]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
-- fork/exec is expensive on Windows, for each time we ask GCC for a library we
-- have to eat the cost of af least 3 of these: gcc -> real_gcc -> cc1.
-- So instead get a list of location that GCC would search and use findDirs
-- which hopefully is written in an optimized manner to take advantage of
-- caching. At the very least we remove the overhead of the fork/exec and waits
-- which dominate a large percentage of startup time on Windows.
getGccSearchDirectory :: Logger -> DynFlags -> String -> IO [FilePath]
getGccSearchDirectory logger dflags key = do
    cache <- readIORef gccSearchDirCache
    case lookup key cache of
      Just x  -> return x
      Nothing -> do
        str <- askLd logger dflags [Option "--print-search-dirs"]
        let line = dropWhile isSpace str
            name = key ++ ": ="
        if null line
          then return []
          else do let val = split $ find name line
                  dirs <- filterM doesDirectoryExist val
                  modifyIORef' gccSearchDirCache ((key, dirs):)
                  return val
      where split :: FilePath -> [FilePath]
            split r = case break (`elem` [';', ':']) r of
                        (s, []    ) -> [s]
                        (s, (_:xs)) -> s : split xs

            find :: String -> String -> String
            find r x = let lst = lines x
                           val = filter (r `isPrefixOf`) lst
                       in case val of
                              [] -> []
                              x:_ -> case break (=='=') x of
                                     (_ , [])    -> []
                                     (_, (_:xs)) -> xs

-- | Get a list of system search directories, this to alleviate pressure on
-- the findSysDll function.
getSystemDirectories :: IO [FilePath]
#if defined(mingw32_HOST_OS)
getSystemDirectories = fmap (:[]) getSystemDirectory
#else
getSystemDirectories = return []
#endif

-- | Merge the given list of paths with those in the environment variable
--   given. If the variable does not exist then just return the identity.
addEnvPaths :: String -> [String] -> IO [String]
addEnvPaths name list
  = do -- According to POSIX (chapter 8.3) a zero-length prefix means current
       -- working directory. Replace empty strings in the env variable with
       -- `working_dir` (see also #14695).
       working_dir <- getCurrentDirectory
       values <- lookupEnv name
       case values of
         Nothing  -> return list
         Just arr -> return $ list ++ splitEnv working_dir arr
    where
      splitEnv :: FilePath -> String -> [String]
      splitEnv working_dir value =
        case break (== envListSep) value of
          (x, []    ) ->
            [if null x then working_dir else x]
          (x, (_:xs)) ->
            (if null x then working_dir else x) : splitEnv working_dir xs
#if defined(mingw32_HOST_OS)
      envListSep = ';'
#else
      envListSep = ':'
#endif

-- ----------------------------------------------------------------------------
-- Loading a dynamic library (dlopen()-ish on Unix, LoadLibrary-ish on Win32)


{- **********************************************************************

                Helper functions

  ********************************************************************* -}

maybePutSDoc :: Logger -> SDoc -> IO ()
maybePutSDoc logger s
    = when (logVerbAtLeast logger 2) $
          logMsg logger
              MCInteractive
              noSrcSpan
              $ withPprStyle defaultUserStyle s

maybePutStr :: Logger -> String -> IO ()
maybePutStr logger s = maybePutSDoc logger (text s)

maybePutStrLn :: Logger -> String -> IO ()
maybePutStrLn logger s = maybePutSDoc logger (text s <> text "\n")

-- | see Note [Generating code for top-level string literal bindings]
allocateTopStrings ::
  Interp -> [(Name, ByteString)] -> AddrEnv -> IO AddrEnv
allocateTopStrings interp topStrings prev_env = do
  let (bndrs, strings) = unzip topStrings
  ptrs <- interpCmd interp $ MallocStrings strings
  evaluate $ extendNameEnvList prev_env (zipWith mk_entry bndrs ptrs)
  where
    mk_entry nm ptr = (nm, (nm, AddrPtr ptr))

-- | Given a list of 'ModBreaks' collected from a list of
-- 'CompiledByteCode', allocate the 'BreakArray'.
allocateBreakArrays ::
  Interp ->
  [ModBreaks] ->
  ModuleEnv (ForeignRef BreakArray) ->
  IO (ModuleEnv (ForeignRef BreakArray))
allocateBreakArrays _interp mbs be =
  foldlM
    ( \be0 ModBreaks {..} ->
        evaluate $ extendModuleEnv be0 modBreaks_module modBreaks_flags
    )
    be
    mbs

-- | Given a list of 'ModBreaks' collected from a list of
-- 'CompiledByteCode', allocate the 'CostCentre' arrays when profiling
-- is enabled.
allocateCCS ::
  Interp ->
  [ModBreaks] ->
  ModuleEnv (Array BreakIndex (RemotePtr CostCentre)) ->
  IO (ModuleEnv (Array BreakIndex (RemotePtr CostCentre)))
allocateCCS interp mbs ce
  | interpreterProfiled interp =
      foldlM
        ( \ce0 ModBreaks {..} -> do
            ccs <-
              mkCostCentres
                interp
                (moduleNameString $ moduleName modBreaks_module)
                (elems modBreaks_ccs)
            evaluate $
              extendModuleEnv ce0 modBreaks_module $
                listArray
                  (0, length ccs - 1)
                  ccs
        )
        ce
        mbs
  | otherwise = pure ce
