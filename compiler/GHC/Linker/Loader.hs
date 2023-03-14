{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections, RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
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
   , loadExpr
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
import GHCi.RemoteTypes


import GHC.ByteCode.Linker
import GHC.ByteCode.Asm
import GHC.ByteCode.Types

import GHC.SysTools

import GHC.Types.Basic
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.SrcLoc
import GHC.Types.Unique.DSet
import GHC.Types.Unique.DFM

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Error
import GHC.Utils.Logger
import GHC.Utils.TmpFs

import GHC.Unit.Env
import GHC.Unit.Finder
import GHC.Unit.Module
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.WholeCoreBindings
import GHC.Unit.Module.Deps
import GHC.Unit.Home.ModInfo
import GHC.Unit.State as Packages

import qualified GHC.Data.ShortText as ST
import qualified GHC.Data.Maybe as Maybes
import GHC.Data.FastString

import GHC.Linker.MacOS
import GHC.Linker.Dynamic
import GHC.Linker.Types

-- Standard libraries
import Control.Monad

import qualified Data.Set as Set
import qualified Data.Map as M
import Data.Char (isSpace)
import Data.IORef
import Data.List (intercalate, isPrefixOf, isSuffixOf, nub, partition)
import Data.Maybe
import Control.Concurrent.MVar
import qualified Control.Monad.Catch as MC

import System.FilePath
import System.Directory
import System.IO.Unsafe
import System.Environment (lookupEnv)

#if defined(mingw32_HOST_OS)
import System.Win32.Info (getSystemDirectory)
#endif

import GHC.Utils.Exception

import GHC.Unit.Module.Graph
import GHC.Types.SourceFile
import GHC.Utils.Misc
import GHC.Iface.Load
import GHC.Unit.Home
import Data.Either
import Control.Applicative
import GHC.Iface.Errors.Ppr

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
  where init_pkgs = unitUDFM rtsUnitId (LoadedPkgInfo rtsUnitId [] [] emptyUniqDSet)

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
                     do let sym_to_find = nameToCLabel name "closure"
                        m <- lookupClosure interp (unpackFS sym_to_find)
                        r <- case m of
                          Just hvref -> mkFinalizedHValue interp hvref
                          Nothing -> linkFail "GHC.Linker.Loader.loadName"
                                       (unpackFS sym_to_find)
                        return (pls,(r, links, pkgs))

loadDependencies
  :: Interp
  -> HscEnv
  -> LoaderState
  -> SrcSpan
  -> [Module]
  -> IO (LoaderState, SuccessFlag, [Linkable], PkgsLoaded) -- ^ returns the set of linkables required
loadDependencies interp hsc_env pls span needed_mods = do
--   initLoaderState (hsc_dflags hsc_env) dl
   let dflags = hsc_dflags hsc_env
   -- The interpreter and dynamic linker can only handle object code built
   -- the "normal" way, i.e. no non-std ways like profiling or ticky-ticky.
   -- So here we check the build tag: if we're building a non-standard way
   -- then we need to find & link object files built the "normal" way.
   maybe_normal_osuf <- checkNonStdWay dflags interp span

   -- Find what packages and linkables are required
   (lnks, all_lnks, pkgs, this_pkgs_needed)
      <- getLinkDeps hsc_env pls
           maybe_normal_osuf span needed_mods

   -- Link the packages and modules required
   pls1 <- loadPackages' interp hsc_env pkgs pls
   (pls2, succ) <- loadModuleLinkables interp hsc_env pls1 lnks
   let this_pkgs_loaded = udfmRestrictKeys all_pkgs_loaded $ getUniqDSet trans_pkgs_needed
       all_pkgs_loaded = pkgs_loaded pls2
       trans_pkgs_needed = unionManyUniqDSets (this_pkgs_needed : [ loaded_pkg_trans_deps pkg
                                                                  | pkg_id <- uniqDSetToList this_pkgs_needed
                                                                  , Just pkg <- [lookupUDFM all_pkgs_loaded pkg_id]
                                                                  ])
   return (pls2, succ, all_lnks, this_pkgs_loaded)


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
         Nothing -> maybePutStrLn logger "done"
         Just mm | platformOS platform /= OSDarwin ->
           preloadFailed mm lib_paths lib_spec
         Just mm | otherwise -> do
           -- As a backup, on Darwin, try to also load a .so file
           -- since (apparently) some things install that way - see
           -- ticket #8770.
           let libfile = ("lib" ++ dll_unadorned) <.> "so"
           err2 <- loadDLL interp libfile
           case err2 of
             Nothing -> maybePutStrLn logger "done"
             Just _  -> preloadFailed mm lib_paths lib_spec
      return pls

    DLLPath dll_path -> do
      do maybe_errstr <- loadDLL interp dll_path
         case maybe_errstr of
            Nothing -> maybePutStrLn logger "done"
            Just mm -> preloadFailed mm lib_paths lib_spec
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
                     else if hostIsDynamic
                             then  do pls1 <- dynLoadObjs interp hsc_env pls names
                                      return (True, pls1)
                             else  do mapM_ (loadObj interp) names
                                      return (True, pls)

    preload_static_archive _paths name
       = do b <- doesFileExist name
            if not b then return False
                     else do if hostIsDynamic
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


{- **********************************************************************

                        Link a byte-code expression

  ********************************************************************* -}

-- | Load a single expression, /including/ first loading packages and
-- modules that this expression depends on.
--
-- Raises an IO exception ('ProgramError') if it can't find a compiled
-- version of the dependents to load.
--
loadExpr :: Interp -> HscEnv -> SrcSpan -> UnlinkedBCO -> IO ForeignHValue
loadExpr interp hsc_env span root_ul_bco = do
  -- Initialise the linker (if it's not been done already)
  initLoaderState interp hsc_env

  -- Take lock for the actual work.
  modifyLoaderState interp $ \pls0 -> do
    -- Load the packages and modules required
    (pls, ok, _, _) <- loadDependencies interp hsc_env pls0 span needed_mods
    if failed ok
      then throwGhcExceptionIO (ProgramError "")
      else do
        -- Load the expression itself
        -- Load the necessary packages and linkables
        let le = linker_env pls
            nobreakarray = error "no break array"
            bco_ix = mkNameEnv [(unlinkedBCOName root_ul_bco, 0)]
        resolved <- linkBCO interp le bco_ix nobreakarray root_ul_bco
        [root_hvref] <- createBCOs interp [resolved]
        fhv <- mkFinalizedHValue interp root_hvref
        return (pls, fhv)
  where
     free_names = uniqDSetToList (bcoFreeNames root_ul_bco)

     needed_mods :: [Module]
     needed_mods = [ nameModule n | n <- free_names,
                     isExternalName n,      -- Names from other modules
                     not (isWiredInName n)  -- Exclude wired-in names
                   ]                        -- (see note below)
        -- Exclude wired-in names because we may not have read
        -- their interface files, so getLinkDeps will fail
        -- All wired-in names are in the base package, which we link
        -- by default, so we can safely ignore them here.

dieWith :: DynFlags -> SrcSpan -> SDoc -> IO a
dieWith dflags span msg = throwGhcExceptionIO (ProgramError (showSDoc dflags (mkLocMessage MCFatal span msg)))


checkNonStdWay :: DynFlags -> Interp -> SrcSpan -> IO (Maybe FilePath)
checkNonStdWay _dflags interp _srcspan
  | ExternalInterp {} <- interpInstance interp = return Nothing
    -- with -fexternal-interpreter we load the .o files, whatever way
    -- they were built.  If they were built for a non-std way, then
    -- we will use the appropriate variant of the iserv binary to load them.

-- #if-guard the following equations otherwise the pattern match checker will
-- complain that they are redundant.
#if defined(HAVE_INTERNAL_INTERPRETER)
checkNonStdWay dflags _interp srcspan
  | hostFullWays == targetFullWays = return Nothing
    -- Only if we are compiling with the same ways as GHC is built
    -- with, can we dynamically load those object files. (see #3604)

  | objectSuf_ dflags == normalObjectSuffix && not (null targetFullWays)
  = failNonStd dflags srcspan

  | otherwise = return (Just (hostWayTag ++ "o"))
  where
    targetFullWays = fullWays (ways dflags)
    hostWayTag = case waysTag hostFullWays of
                  "" -> ""
                  tag -> tag ++ "_"

    normalObjectSuffix :: String
    normalObjectSuffix = phaseInputExt StopLn

data Way' = Normal | Prof | Dyn

failNonStd :: DynFlags -> SrcSpan -> IO (Maybe FilePath)
failNonStd dflags srcspan = dieWith dflags srcspan $
  text "Cannot load" <+> pprWay' compWay <+>
     text "objects when GHC is built" <+> pprWay' ghciWay $$
  text "To fix this, either:" $$
  text "  (1) Use -fexternal-interpreter, or" $$
  buildTwiceMsg
    where compWay
            | ways dflags `hasWay` WayDyn  = Dyn
            | ways dflags `hasWay` WayProf = Prof
            | otherwise = Normal
          ghciWay
            | hostIsDynamic = Dyn
            | hostIsProfiled = Prof
            | otherwise = Normal
          buildTwiceMsg = case (ghciWay, compWay) of
            (Normal, Dyn) -> dynamicTooMsg
            (Dyn, Normal) -> dynamicTooMsg
            _ ->
              text "  (2) Build the program twice: once" <+>
                pprWay' ghciWay <> text ", and then" $$
              text "      " <> pprWay' compWay <+>
                text "using -osuf to set a different object file suffix."
          dynamicTooMsg = text "  (2) Use -dynamic-too," <+>
            text "and use -osuf and -dynosuf to set object file suffixes as needed."
          pprWay' :: Way' -> SDoc
          pprWay' way = text $ case way of
            Normal -> "the normal way"
            Prof -> "with -prof"
            Dyn -> "with -dynamic"
#endif

getLinkDeps :: HscEnv
            -> LoaderState
            -> Maybe FilePath                   -- replace object suffixes?
            -> SrcSpan                          -- for error messages
            -> [Module]                         -- If you need these
            -> IO ([Linkable], [Linkable], [UnitId], UniqDSet UnitId)     -- ... then link these first
            -- The module and package dependencies for the needed modules are returned.
            -- See Note [Object File Dependencies]
-- Fails with an IO exception if it can't find enough files

getLinkDeps hsc_env pls replace_osuf span mods
-- Find all the packages and linkables that a set of modules depends on
 = do {
        -- 1.  Find the dependent home-pkg-modules/packages from each iface
        -- (omitting modules from the interactive package, which is already linked)
      ; (mods_s, pkgs_s) <-
          -- Why two code paths here? There is a significant amount of repeated work
          -- performed calculating transitive dependencies
          -- if --make uses the oneShot code path (see MultiLayerModulesTH_* tests)
          if isOneShot (ghcMode dflags)
            then follow_deps (filterOut isInteractiveModule mods)
                              emptyUniqDSet emptyUniqDSet;
            else do
              (pkgs, mmods) <- unzip <$> mapM get_mod_info all_home_mods
              return (catMaybes mmods, unionManyUniqDSets (init_pkg_set : pkgs))

      ; let
        -- 2.  Exclude ones already linked
        --      Main reason: avoid findModule calls in get_linkable
            (mods_needed, links_got) = partitionEithers (map split_mods mods_s)
            pkgs_needed = eltsUDFM $ getUniqDSet pkgs_s `minusUDFM` pkgs_loaded pls

            split_mods mod =
                let is_linked = findModuleLinkable_maybe (objs_loaded pls) mod <|> findModuleLinkable_maybe (bcos_loaded pls) mod
                in case is_linked of
                     Just linkable -> Right linkable
                     Nothing -> Left mod

        -- 3.  For each dependent module, find its linkable
        --     This will either be in the HPT or (in the case of one-shot
        --     compilation) we may need to use maybe_getFileLinkable
      ; let { osuf = objectSuf dflags }
      ; lnks_needed <- mapM (get_linkable osuf) mods_needed

      ; return (lnks_needed, links_got ++ lnks_needed, pkgs_needed, pkgs_s) }
  where
    dflags = hsc_dflags hsc_env
    mod_graph = hsc_mod_graph hsc_env

    -- This code is used in `--make` mode to calculate the home package and unit dependencies
    -- for a set of modules.
    --
    -- It is significantly more efficient to use the shared transitive dependency
    -- calculation than to compute the transitive dependency set in the same manner as oneShot mode.

    -- It is also a matter of correctness to use the module graph so that dependencies between home units
    -- is resolved correctly.
    make_deps_loop :: (UniqDSet UnitId, Set.Set NodeKey) -> [ModNodeKeyWithUid] -> (UniqDSet UnitId, Set.Set NodeKey)
    make_deps_loop found [] = found
    make_deps_loop found@(found_units, found_mods) (nk:nexts)
      | NodeKey_Module nk `Set.member` found_mods = make_deps_loop found nexts
      | otherwise =
        case M.lookup (NodeKey_Module nk) (mgTransDeps mod_graph) of
            Just trans_deps ->
              let deps = Set.insert (NodeKey_Module nk) trans_deps
                  -- See #936 and the ghci.prog007 test for why we have to continue traversing through
                  -- boot modules.
                  todo_boot_mods = [ModNodeKeyWithUid (GWIB mn NotBoot) uid | NodeKey_Module (ModNodeKeyWithUid (GWIB mn IsBoot) uid) <- Set.toList trans_deps]
              in make_deps_loop (found_units, deps `Set.union` found_mods) (todo_boot_mods ++ nexts)
            Nothing ->
              let (ModNodeKeyWithUid _ uid) = nk
              in make_deps_loop (addOneToUniqDSet found_units uid, found_mods) nexts

    mkNk m = ModNodeKeyWithUid (GWIB (moduleName m) NotBoot) (moduleUnitId m)
    (init_pkg_set, all_deps) = make_deps_loop (emptyUniqDSet, Set.empty) $ map mkNk (filterOut isInteractiveModule mods)

    all_home_mods = [with_uid | NodeKey_Module with_uid <- Set.toList all_deps]

    get_mod_info (ModNodeKeyWithUid gwib uid) =
      case lookupHug (hsc_HUG hsc_env) uid (gwib_mod gwib) of
        Just hmi ->
          let iface = (hm_iface hmi)
              mmod = case mi_hsc_src iface of
                      HsBootFile -> link_boot_mod_error (mi_module iface)
                      _ -> return $ Just (mi_module iface)

          in (mkUniqDSet $ Set.toList $ dep_direct_pkgs (mi_deps iface),) <$>  mmod
        Nothing ->
          let err = text "getLinkDeps: Home module not loaded" <+> ppr (gwib_mod gwib) <+> ppr uid
          in throwGhcExceptionIO (ProgramError (showSDoc dflags err))


       -- This code is used in one-shot mode to traverse downwards through the HPT
       -- to find all link dependencies.
       -- The ModIface contains the transitive closure of the module dependencies
       -- within the current package, *except* for boot modules: if we encounter
       -- a boot module, we have to find its real interface and discover the
       -- dependencies of that.  Hence we need to traverse the dependency
       -- tree recursively.  See bug #936, testcase ghci/prog007.
    follow_deps :: [Module]             -- modules to follow
                -> UniqDSet Module         -- accum. module dependencies
                -> UniqDSet UnitId          -- accum. package dependencies
                -> IO ([Module], UniqDSet UnitId) -- result
    follow_deps []     acc_mods acc_pkgs
        = return (uniqDSetToList acc_mods, acc_pkgs)
    follow_deps (mod:mods) acc_mods acc_pkgs
        = do
          mb_iface <- initIfaceCheck (text "getLinkDeps") hsc_env $
                        loadInterface msg mod (ImportByUser NotBoot)
          iface <- case mb_iface of
                    Maybes.Failed err ->
                      let opts   = initIfaceMessageOpts dflags
                          err_txt = missingInterfaceErrorDiagnostic opts err
                      in throwGhcExceptionIO (ProgramError (showSDoc dflags err_txt))
                    Maybes.Succeeded iface -> return iface

          when (mi_boot iface == IsBoot) $ link_boot_mod_error mod

          let
            pkg = moduleUnit mod
            deps  = mi_deps iface

            pkg_deps = dep_direct_pkgs deps
            (boot_deps, mod_deps) = flip partitionWith (Set.toList (dep_direct_mods deps)) $
              \case
                (_, GWIB m IsBoot)  -> Left m
                (_, GWIB m NotBoot) -> Right m

            mod_deps' = case hsc_home_unit_maybe hsc_env of
                          Nothing -> []
                          Just home_unit -> filter (not . (`elementOfUniqDSet` acc_mods)) (map (mkHomeModule home_unit) $ (boot_deps ++ mod_deps))
            acc_mods'  = case hsc_home_unit_maybe hsc_env of
                          Nothing -> acc_mods
                          Just home_unit -> addListToUniqDSet acc_mods (mod : map (mkHomeModule home_unit) mod_deps)
            acc_pkgs'  = addListToUniqDSet acc_pkgs (Set.toList pkg_deps)

          case hsc_home_unit_maybe hsc_env of
            Just home_unit | isHomeUnit home_unit pkg ->  follow_deps (mod_deps' ++ mods)
                                                                      acc_mods' acc_pkgs'
            _ ->  follow_deps mods acc_mods (addOneToUniqDSet acc_pkgs' (toUnitId pkg))
        where
           msg = text "need to link module" <+> ppr mod <+>
                  text "due to use of Template Haskell"



    link_boot_mod_error :: Module -> IO a
    link_boot_mod_error mod =
        throwGhcExceptionIO (ProgramError (showSDoc dflags (
            text "module" <+> ppr mod <+>
            text "cannot be linked; it is only available as a boot module")))

    no_obj :: Outputable a => a -> IO b
    no_obj mod = dieWith dflags span $
                     text "cannot find object file for module " <>
                        quotes (ppr mod) $$
                     while_linking_expr

    while_linking_expr = text "while linking an interpreted expression"


    -- See Note [Using Byte Code rather than Object Code for Template Haskell]
    homeModLinkable :: DynFlags -> HomeModInfo -> Maybe Linkable
    homeModLinkable dflags hmi =
      if gopt Opt_UseBytecodeRatherThanObjects dflags
        then homeModInfoByteCode hmi <|> homeModInfoObject hmi
        else homeModInfoObject hmi   <|> homeModInfoByteCode hmi

    get_linkable osuf mod      -- A home-package module
        | Just mod_info <- lookupHugByModule mod (hsc_HUG hsc_env)
        = adjust_linkable (Maybes.expectJust "getLinkDeps" (homeModLinkable dflags mod_info))
        | otherwise
        = do    -- It's not in the HPT because we are in one shot mode,
                -- so use the Finder to get a ModLocation...
             case hsc_home_unit_maybe hsc_env of
              Nothing -> no_obj mod
              Just home_unit -> do

                let fc = hsc_FC hsc_env
                let dflags = hsc_dflags hsc_env
                let fopts = initFinderOpts dflags
                mb_stuff <- findHomeModule fc fopts home_unit (moduleName mod)
                case mb_stuff of
                  Found loc mod -> found loc mod
                  _ -> no_obj (moduleName mod)
        where
            found loc mod = do {
                -- ...and then find the linkable for it
               mb_lnk <- findObjectLinkableMaybe mod loc ;
               case mb_lnk of {
                  Nothing  -> no_obj mod ;
                  Just lnk -> adjust_linkable lnk
              }}

            adjust_linkable lnk
                | Just new_osuf <- replace_osuf = do
                        new_uls <- mapM (adjust_ul new_osuf)
                                        (linkableUnlinked lnk)
                        return lnk{ linkableUnlinked=new_uls }
                | otherwise =
                        return lnk

            adjust_ul new_osuf (DotO file) = do
                massert (osuf `isSuffixOf` file)
                let file_base = fromJust (stripExtension osuf file)
                    new_file = file_base <.> new_osuf
                ok <- doesFileExist new_file
                if (not ok)
                   then dieWith dflags span $
                          text "cannot find object file "
                                <> quotes (text new_file) $$ while_linking_expr
                   else return (DotO new_file)
            adjust_ul _ (DotA fp) = panic ("adjust_ul DotA " ++ show fp)
            adjust_ul _ (DotDLL fp) = panic ("adjust_ul DotDLL " ++ show fp)
            adjust_ul _ l@(BCOs {}) = return l
            adjust_ul _ l@LoadedBCOs{} = return l
            adjust_ul _ (CoreBindings (WholeCoreBindings _ mod _))     = pprPanic "Unhydrated core bindings" (ppr mod)

{-
Note [Using Byte Code rather than Object Code for Template Haskell]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `-fprefer-byte-code` flag allows a user to specify that they want to use
byte code (if availble) rather than object code for home module dependenices
when executing Template Haskell splices.

Why might you want to use byte code rather than object code?

* Producing object code is much slower than producing byte code (for example if you're using -fno-code)
* Linking many large object files, which happens once per splice, is quite expensive. (#21700)

So we allow the user to choose to use byte code rather than object files if they want to avoid these
two pitfalls.

When using `-fprefer-byte-code` you have to arrange to have the byte code availble.
In normal --make mode it will not be produced unless you enable `-fbyte-code-and-object-code`.
See Note [Home module build products] for some more information about that.

The only other place where the flag is consulted is when enabling code generation
with `-fno-code`, which does so to anticipate what decision we will make at the
splice point about what we would prefer.

-}

{- **********************************************************************

              Loading a Decls statement

  ********************************************************************* -}

loadDecls :: Interp -> HscEnv -> SrcSpan -> CompiledByteCode -> IO ([(Name, ForeignHValue)], [Linkable], PkgsLoaded)
loadDecls interp hsc_env span cbc@CompiledByteCode{..} = do
    -- Initialise the linker (if it's not been done already)
    initLoaderState interp hsc_env

    -- Take lock for the actual work.
    modifyLoaderState interp $ \pls0 -> do
      -- Link the packages and modules required
      (pls, ok, links_needed, units_needed) <- loadDependencies interp hsc_env pls0 span needed_mods
      if failed ok
        then throwGhcExceptionIO (ProgramError "")
        else do
          -- Link the expression itself
          let le  = linker_env pls
              le2 = le { itbl_env = plusNameEnv (itbl_env le) bc_itbls
                       , addr_env = plusNameEnv (addr_env le) bc_strs }

          -- Link the necessary packages and linkables
          new_bindings <- linkSomeBCOs interp le2 [cbc]
          nms_fhvs <- makeForeignNamedHValueRefs interp new_bindings
          let ce2  = extendClosureEnv (closure_env le2) nms_fhvs
              !pls2 = pls { linker_env = le2 { closure_env = ce2 } }
          return (pls2, (nms_fhvs, links_needed, units_needed))
  where
    free_names = uniqDSetToList $
      foldr (unionUniqDSets . bcoFreeNames) emptyUniqDSet bc_bcos

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

        let (objs, bcos) = partition isObjectLinkable
                              (concatMap partitionLinkable linkables)

                -- Load objects first; they can't depend on BCOs
        (pls1, ok_flag) <- loadObjects interp hsc_env pls objs

        if failed ok_flag then
                return (pls1, Failed)
          else do
                pls2 <- dynLinkBCOs interp pls1 bcos
                return (pls2, Succeeded)


-- HACK to support f-x-dynamic in the interpreter; no other purpose
partitionLinkable :: Linkable -> [Linkable]
partitionLinkable li
   = let li_uls = linkableUnlinked li
         li_uls_obj = filter isObject li_uls
         li_uls_bco = filter isInterpretable li_uls
     in
         case (li_uls_obj, li_uls_bco) of
            (_:_, _:_) -> [li {linkableUnlinked=li_uls_obj},
                           li {linkableUnlinked=li_uls_bco}]
            _ -> [li]

findModuleLinkable_maybe :: LinkableSet -> Module -> Maybe Linkable
findModuleLinkable_maybe = lookupModuleEnv

linkableInSet :: Linkable -> LinkableSet -> Bool
linkableInSet l objs_loaded =
  case findModuleLinkable_maybe objs_loaded (linkableModule l) of
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
            unlinkeds                = concatMap linkableUnlinked new_objs
            wanted_objs              = map nameOfObject unlinkeds

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
                      -- Even if we're e.g. profiling, we still want
                      -- the vanilla dynamic libraries, so we set the
                      -- ways / build tag to be just WayDyn.
                      targetWays_ = Set.singleton WayDyn,
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
        Nothing -> return $! pls { temp_sos = (libPath, libName) : temp_sos }
        Just err -> linkFail msg err
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
            unlinkeds :: [Unlinked]
            unlinkeds                = concatMap linkableUnlinked new_bcos

            cbcs :: [CompiledByteCode]
            cbcs      = concatMap byteCodeOfObject unlinkeds


            le1 = linker_env pls
            ie2 = foldr plusNameEnv (itbl_env le1) (map bc_itbls cbcs)
            ae2 = foldr plusNameEnv (addr_env le1) (map bc_strs cbcs)
            le2 = le1 { itbl_env = ie2, addr_env = ae2 }

        names_and_refs <- linkSomeBCOs interp le2 cbcs

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
             -> LinkerEnv
             -> [CompiledByteCode]
             -> IO [(Name,HValueRef)]
                        -- The returned HValueRefs are associated 1-1 with
                        -- the incoming unlinked BCOs.  Each gives the
                        -- value of the corresponding unlinked BCO

linkSomeBCOs interp le mods = foldr fun do_link mods []
 where
  fun CompiledByteCode{..} inner accum =
    case bc_breaks of
      Nothing -> inner ((panic "linkSomeBCOs: no break array", bc_bcos) : accum)
      Just mb -> withForeignRef (modBreaks_flags mb) $ \breakarray ->
                   inner ((breakarray, bc_bcos) : accum)

  do_link [] = return []
  do_link mods = do
    let flat = [ (breakarray, bco) | (breakarray, bcos) <- mods, bco <- bcos ]
        names = map (unlinkedBCOName . snd) flat
        bco_ix = mkNameEnv (zip names [0..])
    resolved <- sequence [ linkBCO interp le bco_ix breakarray bco
                         | (breakarray, bco) <- flat ]
    hvrefs <- createBCOs interp resolved
    return (zip names hvrefs)

-- | Useful to apply to the result of 'linkSomeBCOs'
makeForeignNamedHValueRefs
  :: Interp -> [(Name,HValueRef)] -> IO [(Name,ForeignHValue)]
makeForeignNamedHValueRefs interp bindings =
  mapM (\(n, hvref) -> (n,) <$> mkFinalizedHValue interp hvref) bindings

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

  let (objs_to_keep', bcos_to_keep') = partition isObjectLinkable keep_linkables
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
      = mapM_ (unloadObj interp) [f | DotO f <- linkableUnlinked lnk]
                -- The components of a BCO linkable may contain
                -- dot-o files.  Which is very confusing.
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
             ; (hs_cls, extra_cls) <- loadPackage interp hsc_env pkg_cfg
             ; let trans_deps = unionManyUniqDSets [ addOneToUniqDSet (loaded_pkg_trans_deps loaded_pkg_info) dep_pkg
                                                   | dep_pkg <- deps
                                                   , Just loaded_pkg_info <- pure (lookupUDFM pkgs' dep_pkg)
                                                   ]
             ; return (addToUDFM pkgs' new_pkg (LoadedPkgInfo new_pkg hs_cls extra_cls trans_deps)) }

        | otherwise
        = throwGhcExceptionIO (CmdLineError ("unknown package: " ++ unpackFS (unitIdFS new_pkg)))


loadPackage :: Interp -> HscEnv -> UnitInfo -> IO ([LibrarySpec], [LibrarySpec])
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
        let known_dlls = [ dll  | DLLPath dll    <- classifieds ]
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
        mapM_ (load_dyn interp hsc_env True) known_dlls
        -- For remaining `dlls` crash early only when there is surely
        -- no package's DLL around ... (not is_dyn)
        mapM_ (load_dyn interp hsc_env (not is_dyn) . platformSOName platform) dlls
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
             return (hs_classifieds, extra_classifieds)
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
load_dyn :: Interp -> HscEnv -> Bool -> FilePath -> IO ()
load_dyn interp hsc_env crash_early dll = do
  r <- loadDLL interp dll
  case r of
    Nothing  -> return ()
    Just err ->
      if crash_early
        then cmdLineErrorIO err
        else
          when (diag_wopt Opt_WarnMissedExtraSharedLib diag_opts)
            $ logMsg logger
                (mkMCDiagnostic diag_opts (WarningWithFlag Opt_WarnMissedExtraSharedLib) Nothing)
                  noSrcSpan $ withPprStyle defaultUserStyle (note err)
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

     hs_dyn_lib_name = lib ++ dynLibSuffix (ghcNameVersion dflags)
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
  = case os of
      OSMinGW32 ->
        do gcc_dirs <- getGccSearchDirectory logger dflags "libraries"
           sys_dirs <- getSystemDirectories
           return $ nub $ gcc_dirs ++ sys_dirs
      _         -> return []

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
            split r = case break (==';') r of
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
