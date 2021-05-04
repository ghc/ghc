{-# LANGUAGE CPP, TupleSections, RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
   -- * Misc
   , extendLoadedPkgs
   )
where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Settings

import GHC.Platform
import GHC.Platform.Ways

import GHC.Driver.Phases
import GHC.Driver.Env
import GHC.Driver.Session
import GHC.Driver.Ppr
import GHC.Driver.Dependencies

import GHC.Tc.Utils.Monad

import GHC.Runtime.Interpreter
import GHCi.RemoteTypes


import GHC.ByteCode.Linker
import GHC.ByteCode.Asm
import GHC.ByteCode.Types



import GHC.Types.Basic
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.SrcLoc
import GHC.Types.Unique.DSet

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Error
import GHC.Utils.Logger
import GHC.Utils.TmpFs

import GHC.Unit.Env
import GHC.Unit.Module
import GHC.Unit.State as Packages

import qualified GHC.Data.ShortText as ST
import GHC.Data.FastString

import GHC.Linker.MacOS
import GHC.Linker.Dynamic
import GHC.Linker.Types

-- Standard libraries
import Control.Monad

import qualified Data.Set as Set
import Data.List ( partition, intercalate, nub )
import Data.Maybe
import Control.Concurrent.MVar
import qualified Control.Monad.Catch as MC

import System.FilePath
import System.Directory

#if defined(mingw32_HOST_OS)
import System.Win32.Info (getSystemDirectory)
#endif

import GHC.Utils.Exception

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

emptyLoaderState :: LoaderState
emptyLoaderState = LoaderState
   { closure_env = emptyNameEnv
   , itbl_env    = emptyNameEnv
   , pkgs_loaded = init_pkgs
   , bcos_loaded = []
   , objs_loaded = []
   , temp_sos = []
   }
  -- Packages that don't need loading, because the compiler
  -- shares them with the interpreted program.
  --
  -- The linker's symbol table is populated with RTS symbols using an
  -- explicit list.  See rts/Linker.c for details.
  where init_pkgs = [rtsUnitId]

extendLoadedPkgs :: Interp -> [UnitId] -> IO ()
extendLoadedPkgs interp pkgs =
  modifyLoaderState_ interp $ \s ->
      return s{ pkgs_loaded = pkgs ++ pkgs_loaded s }

extendLoadedEnv :: Interp -> [(Name,ForeignHValue)] -> IO ()
extendLoadedEnv interp new_bindings =
  modifyLoaderState_ interp $ \pls@LoaderState{..} -> do
    let new_ce = extendClosureEnv closure_env new_bindings
    return $! pls{ closure_env = new_ce }
    -- strictness is important for not retaining old copies of the pls

deleteFromLoadedEnv :: Interp -> [Name] -> IO ()
deleteFromLoadedEnv interp to_remove =
  modifyLoaderState_ interp $ \pls -> do
    let ce = closure_env pls
    let new_ce = delListFromNameEnv ce to_remove
    return pls{ closure_env = new_ce }

-- | Load the module containing the given Name and get its associated 'HValue'.
--
-- Throws a 'ProgramError' if loading fails or the name cannot be found.
loadName :: Interp -> HscEnv -> Name -> IO ForeignHValue
loadName interp hsc_env name = do
  initLoaderState interp hsc_env
  modifyLoaderState interp $ \pls0 -> do
    pls <- if not (isExternalName name)
       then return pls0
       else do
         (pls', ok) <- loadDependencies interp hsc_env pls0 noSrcSpan
                          [nameModule name]
         if failed ok
           then throwGhcExceptionIO (ProgramError "")
           else return pls'

    case lookupNameEnv (closure_env pls) name of
      Just (_,aa) -> return (pls,aa)
      Nothing     -> ASSERT2(isExternalName name, ppr name)
                     do let sym_to_find = nameToCLabel name "closure"
                        m <- lookupClosure interp (unpackFS sym_to_find)
                        r <- case m of
                          Just hvref -> mkFinalizedHValue interp hvref
                          Nothing -> linkFail "GHC.Linker.Loader.loadName"
                                       (unpackFS sym_to_find)
                        return (pls,r)

loadDependencies
  :: Interp
  -> HscEnv
  -> LoaderState
  -> SrcSpan -> [Module]
  -> IO (LoaderState, SuccessFlag)
loadDependencies interp hsc_env pls span needed_mods = do
--   initLoaderState (hsc_dflags hsc_env) dl
   let hpt = hsc_HPT hsc_env
   let dflags = hsc_dflags hsc_env
   -- The interpreter and dynamic linker can only handle object code built
   -- the "normal" way, i.e. no non-std ways like profiling or ticky-ticky.
   -- So here we check the build tag: if we're building a non-standard way
   -- then we need to find & link object files built the "normal" way.
   maybe_normal_osuf <- checkNonStdWay dflags interp span

   -- Find what packages and linkables are required
   -- (omitting modules from the interactive package, which is already linked)
   (lnks, pkgs) <- getLinkDeps (text "linking") hsc_env hpt (pkgs_loaded pls, objs_loaded pls, bcos_loaded pls)
                               maybe_normal_osuf span (filterOut isInteractiveModule needed_mods) []

   -- Link the packages and modules required
   pls1 <- loadPackages' interp hsc_env pkgs pls
   loadModules interp hsc_env pls1 lnks


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
            modifyLoaderState_ interp $ \pls ->
                let cur = closure_env pls
                    new = delListFromNameEnv cur (map fst new_env)
                in return pls{ closure_env = new }


-- | Display the loader state.
showLoaderState :: Interp -> IO SDoc
showLoaderState interp = do
  ls <- readMVar (loader_state (interpLoader interp))
  let docs = case ls of
        Nothing  -> [ text "Loader not initialised"]
        Just pls -> [ text "Pkgs:" <+> ppr (pkgs_loaded pls)
                    , text "Objs:" <+> ppr (objs_loaded pls)
                    , text "BCOs:" <+> ppr (bcos_loaded pls)
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
  pls <- loadPackages' interp hsc_env (preloadUnits (hsc_units hsc_env)) pls0

  -- steps (c), (d) and (e)
  loadCmdLineLibs' interp hsc_env pls


loadCmdLineLibs :: Interp -> HscEnv -> IO ()
loadCmdLineLibs interp hsc_env = do
  initLoaderState interp hsc_env
  modifyLoaderState_ interp $ \pls ->
    loadCmdLineLibs' interp hsc_env pls

loadCmdLineLibs'
  :: Interp
  -> HscEnv
  -> LoaderState
  -> IO LoaderState
loadCmdLineLibs' interp hsc_env pls =
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

      maybePutStrLn logger dflags "Search directories (user):"
      maybePutStr logger dflags (unlines $ map ("  "++) lib_paths_env)
      maybePutStrLn logger dflags "Search directories (gcc):"
      maybePutStr logger dflags (unlines $ map ("  "++) gcc_paths)

      libspecs
        <- mapM (locateLib interp hsc_env False lib_paths_env gcc_paths) minus_ls

      -- (d) Link .o files from the command-line
      classified_ld_inputs <- mapM (classifyLdInput logger dflags)
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

           maybePutStr logger dflags "final link ... "
           ok <- resolveObjs interp

           -- DLLs are loaded, reset the search paths
           mapM_ (removeLibrarySearchPath interp) $ reverse pathCache

           if succeeded ok then maybePutStrLn logger dflags "done"
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

classifyLdInput :: Logger -> DynFlags -> FilePath -> IO (Maybe LibrarySpec)
classifyLdInput logger dflags f
  | isObjectFilename platform f = return (Just (Objects [f]))
  | isDynLibFilename platform f = return (Just (DLLPath f))
  | otherwise          = do
        putLogMsg logger dflags MCInfo noSrcSpan
            $ withPprStyle defaultUserStyle
            (text ("Warning: ignoring unrecognised input `" ++ f ++ "'"))
        return Nothing
    where platform = targetPlatform dflags

preloadLib
  :: Interp
  -> HscEnv
  -> [String]
  -> [String]
  -> LoaderState
  -> LibrarySpec
  -> IO LoaderState
preloadLib interp hsc_env lib_paths framework_paths pls lib_spec = do
  maybePutStr logger dflags ("Loading object " ++ showLS lib_spec ++ " ... ")
  case lib_spec of
    Objects static_ishs -> do
      (b, pls1) <- preload_statics lib_paths static_ishs
      maybePutStrLn logger dflags (if b  then "done" else "not found")
      return pls1

    Archive static_ish -> do
      b <- preload_static_archive lib_paths static_ish
      maybePutStrLn logger dflags (if b  then "done" else "not found")
      return pls

    DLL dll_unadorned -> do
      maybe_errstr <- loadDLL interp (platformSOName platform dll_unadorned)
      case maybe_errstr of
         Nothing -> maybePutStrLn logger dflags "done"
         Just mm | platformOS platform /= OSDarwin ->
           preloadFailed mm lib_paths lib_spec
         Just mm | otherwise -> do
           -- As a backup, on Darwin, try to also load a .so file
           -- since (apparently) some things install that way - see
           -- ticket #8770.
           let libfile = ("lib" ++ dll_unadorned) <.> "so"
           err2 <- loadDLL interp libfile
           case err2 of
             Nothing -> maybePutStrLn logger dflags "done"
             Just _  -> preloadFailed mm lib_paths lib_spec
      return pls

    DLLPath dll_path -> do
      do maybe_errstr <- loadDLL interp dll_path
         case maybe_errstr of
            Nothing -> maybePutStrLn logger dflags "done"
            Just mm -> preloadFailed mm lib_paths lib_spec
         return pls

    Framework framework ->
      if platformUsesFrameworks (targetPlatform dflags)
      then do maybe_errstr <- loadFramework interp framework_paths framework
              case maybe_errstr of
                 Nothing -> maybePutStrLn logger dflags "done"
                 Just mm -> preloadFailed mm framework_paths lib_spec
              return pls
      else throwGhcExceptionIO (ProgramError "preloadLib Framework")

  where
    dflags = hsc_dflags hsc_env
    logger = hsc_logger hsc_env

    platform = targetPlatform dflags

    preloadFailed :: String -> [String] -> LibrarySpec -> IO ()
    preloadFailed sys_errmsg paths spec
       = do maybePutStr logger dflags "failed.\n"
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
    (pls, ok) <- loadDependencies interp hsc_env pls0 span needed_mods
    if failed ok
      then throwGhcExceptionIO (ProgramError "")
      else do
        -- Load the expression itself
        let ie = itbl_env pls
            ce = closure_env pls

        -- Load the necessary packages and linkables
        let nobreakarray = error "no break array"
            bco_ix = mkNameEnv [(unlinkedBCOName root_ul_bco, 0)]
        resolved <- linkBCO interp ie ce bco_ix nobreakarray root_ul_bco
        [root_hvref] <- createBCOs interp dflags [resolved]
        fhv <- mkFinalizedHValue interp root_hvref
        return (pls, fhv)
  where
     dflags = hsc_dflags hsc_env
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
checkNonStdWay dflags interp srcspan
  | ExternalInterp {} <- interpInstance interp = return Nothing
    -- with -fexternal-interpreter we load the .o files, whatever way
    -- they were built.  If they were built for a non-std way, then
    -- we will use the appropriate variant of the iserv binary to load them.

  | hostFullWays == targetFullWays = return Nothing
    -- Only if we are compiling with the same ways as GHC is built
    -- with, can we dynamically load those object files. (see #3604)

  | objectSuf dflags == normalObjectSuffix && not (null targetFullWays)
  = failNonStd dflags srcspan

  | otherwise = return (Just (hostWayTag ++ "o"))
  where
    targetFullWays = fullWays (ways dflags)
    hostWayTag = case waysTag hostFullWays of
                  "" -> ""
                  tag -> tag ++ "_"

normalObjectSuffix :: String
normalObjectSuffix = phaseInputExt StopLn

failNonStd :: DynFlags -> SrcSpan -> IO (Maybe FilePath)
failNonStd dflags srcspan = dieWith dflags srcspan $
  text "Cannot load" <+> compWay <+>
     text "objects when GHC is built" <+> ghciWay $$
  text "To fix this, either:" $$
  text "  (1) Use -fexternal-interpreter, or" $$
  text "  (2) Build the program twice: once" <+>
                       ghciWay <> text ", and then" $$
  text "      with" <+> compWay <+>
     text "using -osuf to set a different object file suffix."
    where compWay
            | WayDyn `elem` ways dflags = text "-dynamic"
            | WayProf `elem` ways dflags = text "-prof"
            | otherwise = text "normal"
          ghciWay
            | hostIsDynamic = text "with -dynamic"
            | hostIsProfiled = text "with -prof"
            | otherwise = text "the normal way"




{- **********************************************************************

              Loading a Decls statement

  ********************************************************************* -}

loadDecls :: Interp -> HscEnv -> SrcSpan -> CompiledByteCode -> IO ()
loadDecls interp hsc_env span cbc@CompiledByteCode{..} = do
    -- Initialise the linker (if it's not been done already)
    initLoaderState interp hsc_env

    -- Take lock for the actual work.
    modifyLoaderState_ interp $ \pls0 -> do
      -- Link the packages and modules required
      (pls, ok) <- loadDependencies interp hsc_env pls0 span needed_mods
      if failed ok
        then throwGhcExceptionIO (ProgramError "")
        else do
          -- Link the expression itself
          let ie = plusNameEnv (itbl_env pls) bc_itbls
              ce = closure_env pls

          -- Link the necessary packages and linkables
          new_bindings <- linkSomeBCOs dflags interp ie ce [cbc]
          nms_fhvs <- makeForeignNamedHValueRefs interp new_bindings
          let pls2 = pls { closure_env = extendClosureEnv ce nms_fhvs
                         , itbl_env    = ie }
          return pls2
  where
    dflags = hsc_dflags hsc_env
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
    (pls', ok) <- loadDependencies interp hsc_env pls noSrcSpan [mod]
    if failed ok
      then throwGhcExceptionIO (ProgramError "could not load module")
      else return pls'

{- **********************************************************************

                Link some linkables
        The linkables may consist of a mixture of
        byte-code modules and object modules

  ********************************************************************* -}

loadModules :: Interp -> HscEnv -> LoaderState -> [Linkable] -> IO (LoaderState, SuccessFlag)
loadModules interp hsc_env pls linkables
  = mask_ $ do  -- don't want to be interrupted by ^C in here

        let (objs, bcos) = partition isObjectLinkable
                              (concatMap partitionLinkable linkables)
        let dflags = hsc_dflags hsc_env

                -- Load objects first; they can't depend on BCOs
        (pls1, ok_flag) <- loadObjects interp hsc_env pls objs

        if failed ok_flag then
                return (pls1, Failed)
          else do
                pls2 <- dynLinkBCOs dflags interp pls1 bcos
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

findModuleLinkable_maybe :: [Linkable] -> Module -> Maybe Linkable
findModuleLinkable_maybe lis mod
   = case [LM time nm us | LM time nm us <- lis, nm == mod] of
        []   -> Nothing
        [li] -> Just li
        _    -> pprPanic "findModuleLinkable" (ppr mod)

linkableInSet :: Linkable -> [Linkable] -> Bool
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
      newTempLibName logger tmpfs dflags TFL_CurrentModule (platformSOExt platform)
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
                                          : if gopt Opt_RPath dflags
                                            then [ Option "-Xlinker"
                                                 , Option "-rpath"
                                                 , Option "-Xlinker"
                                                 , Option lp ]
                                            else [])
                                     (nub $ fst <$> temp_sos)
                        ++ concatMap
                             (\lp -> Option ("-L" ++ lp)
                                  : if gopt Opt_RPath dflags
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
    linkDynLib logger tmpfs dflags2 unit_env objs pkgs_loaded

    -- if we got this far, extend the lifetime of the library file
    changeTempFilesLifetime tmpfs TFL_GhcSession [soFile]
    m <- loadDLL interp soFile
    case m of
        Nothing -> return $! pls { temp_sos = (libPath, libName) : temp_sos }
        Just err -> linkFail msg err
  where
    msg = "GHC.Linker.Loader.dynLoadObjs: Loading temp shared object failed"

rmDupLinkables :: [Linkable]    -- Already loaded
               -> [Linkable]    -- New linkables
               -> ([Linkable],  -- New loaded set (including new ones)
                   [Linkable])  -- New linkables (excluding dups)
rmDupLinkables already ls
  = go already [] ls
  where
    go already extras [] = (already, extras)
    go already extras (l:ls)
        | linkableInSet l already = go already     extras     ls
        | otherwise               = go (l:already) (l:extras) ls

{- **********************************************************************

                The byte-code linker

  ********************************************************************* -}


dynLinkBCOs :: DynFlags -> Interp -> LoaderState -> [Linkable] -> IO LoaderState
dynLinkBCOs dflags interp pls bcos = do

        let (bcos_loaded', new_bcos) = rmDupLinkables (bcos_loaded pls) bcos
            pls1                     = pls { bcos_loaded = bcos_loaded' }
            unlinkeds :: [Unlinked]
            unlinkeds                = concatMap linkableUnlinked new_bcos

            cbcs :: [CompiledByteCode]
            cbcs      = map byteCodeOfObject unlinkeds


            ies        = map bc_itbls cbcs
            gce       = closure_env pls
            final_ie  = foldr plusNameEnv (itbl_env pls) ies

        names_and_refs <- linkSomeBCOs dflags interp final_ie gce cbcs

        -- We only want to add the external ones to the ClosureEnv
        let (to_add, to_drop) = partition (isExternalName.fst) names_and_refs

        -- Immediately release any HValueRefs we're not going to add
        freeHValueRefs interp (map snd to_drop)
        -- Wrap finalizers on the ones we want to keep
        new_binds <- makeForeignNamedHValueRefs interp to_add

        return pls1 { closure_env = extendClosureEnv gce new_binds,
                      itbl_env    = final_ie }

-- Link a bunch of BCOs and return references to their values
linkSomeBCOs :: DynFlags
             -> Interp
             -> ItblEnv
             -> ClosureEnv
             -> [CompiledByteCode]
             -> IO [(Name,HValueRef)]
                        -- The returned HValueRefs are associated 1-1 with
                        -- the incoming unlinked BCOs.  Each gives the
                        -- value of the corresponding unlinked BCO

linkSomeBCOs dflags interp ie ce mods = foldr fun do_link mods []
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
    resolved <- sequence [ linkBCO interp ie ce bco_ix breakarray bco
                         | (breakarray, bco) <- flat ]
    hvrefs <- createBCOs interp dflags resolved
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

        let dflags = hsc_dflags hsc_env
        let logger = hsc_logger hsc_env
        debugTraceMsg logger dflags 3 $
          text "unload: retaining objs" <+> ppr (objs_loaded new_pls)
        debugTraceMsg logger dflags 3 $
          text "unload: retaining bcos" <+> ppr (bcos_loaded new_pls)
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

  let (objs_to_keep, bcos_to_keep) = partition isObjectLinkable keep_linkables

      discard keep l = not (linkableInSet l keep)

      (objs_to_unload, remaining_objs_loaded) =
         partition (discard objs_to_keep) objs_loaded
      (bcos_to_unload, remaining_bcos_loaded) =
         partition (discard bcos_to_keep) bcos_loaded

  mapM_ unloadObjs objs_to_unload
  mapM_ unloadObjs bcos_to_unload

  -- If we unloaded any object files at all, we need to purge the cache
  -- of lookupSymbol results.
  when (not (null (objs_to_unload ++
                   filter (not . null . linkableObjs) bcos_to_unload))) $
    purgeLookupSymbolCache interp

  let !bcos_retained = mkModuleSet $ map linkableModule remaining_bcos_loaded

      -- Note that we want to remove all *local*
      -- (i.e. non-isExternal) names too (these are the
      -- temporary bindings from the command line).
      keep_name :: (Name, a) -> Bool
      keep_name (n,_) = isExternalName n &&
                        nameModule n `elemModuleSet` bcos_retained

      itbl_env'     = filterNameEnv keep_name itbl_env
      closure_env'  = filterNameEnv keep_name closure_env

      !new_pls = pls { itbl_env = itbl_env',
                       closure_env = closure_env',
                       bcos_loaded = remaining_bcos_loaded,
                       objs_loaded = remaining_objs_loaded }

  return new_pls
  where
    unloadObjs :: Linkable -> IO ()
    unloadObjs lnk
        -- The RTS's PEi386 linker currently doesn't support unloading.
      | isWindowsHost = return ()

      | hostIsDynamic = return ()
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
  modifyLoaderState_ interp $ \pls -> do
    loadPackages' interp hsc_env new_pkgs pls

loadPackages' :: Interp -> HscEnv -> [UnitId] -> LoaderState -> IO LoaderState
loadPackages' interp hsc_env new_pkgs pls = do
    (_, pkgs') <- loadPackagesX (loadPackage interp hsc_env) hsc_env new_pkgs (pkgs_loaded pls)
    return $! pls { pkgs_loaded = pkgs' }





loadPackage :: Interp -> HscEnv -> UnitInfo -> IO ()
loadPackage interp hsc_env pkg = do
        let dflags    = hsc_dflags hsc_env
        let logger    = hsc_logger hsc_env
            platform  = targetPlatform dflags
            is_dyn    = interpreterDynamic interp
            dirs | is_dyn    = map ST.unpack $ Packages.unitLibraryDynDirs pkg
                 | otherwise = map ST.unpack $ Packages.unitLibraryDirs pkg
        classifieds <- computePackageDeps interp hsc_env pkg
        -- Complication: all the .so's must be loaded before any of the .o's.
        let known_dlls = [ dll  | DLLPath dll    <- classifieds ]
            dlls       = [ dll  | DLL dll        <- classifieds ]
            objs       = [ obj  | Objects objs    <- classifieds
                                , obj <- objs ]
            archs      = [ arch | Archive arch   <- classifieds ]
        -- Add directories to library search paths
        let dll_paths  = map takeDirectory known_dlls
            all_paths  = nub $ map normalise $ dll_paths ++ dirs
        all_paths_env <- addEnvPaths "LD_LIBRARY_PATH" all_paths
        pathCache <- mapM (addLibrarySearchPath interp) all_paths_env

        maybePutSDoc logger dflags
            (text "Loading unit " <> pprUnitInfoForUser pkg <> text " ... ")

        -- See comments with partOfGHCi
#if defined(CAN_LOAD_DLL)
        when (unitPackageName pkg `notElem` partOfGHCi) $ do
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

        maybePutStr logger dflags "linking ... "
        ok <- resolveObjs interp

        -- DLLs are loaded, reset the search paths
        -- Import libraries will be loaded via loadArchive so only
        -- reset the DLL search path after all archives are loaded
        -- as well.
        mapM_ (removeLibrarySearchPath interp) $ reverse pathCache

        if succeeded ok
           then maybePutStrLn logger dflags "done."
           else let errmsg = text "unable to load unit `"
                             <> pprUnitInfoForUser pkg <> text "'"
                 in throwGhcExceptionIO (InstallationError (showSDoc dflags errmsg))

-- If this package is already part of the GHCi binary, we'll already
-- have the right DLLs for this package loaded, so don't try to
-- load them again.
--
-- But on Win32 we must load them 'again'; doing so is a harmless no-op
-- as far as the loader is concerned, but it does initialise the list
-- of DLL handles that rts/Linker.c maintains, and that in turn is
-- used by lookupSymbol.  So we must call addDLL for each library
-- just to get the DLL handle into the list.
partOfGHCi :: [PackageName]
partOfGHCi
 | isWindowsHost || isDarwinHost = []
 | otherwise = map (PackageName . mkFastString)
                   ["base", "template-haskell", "editline"]
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
          when (wopt Opt_WarnMissedExtraSharedLib dflags)
            $ putLogMsg logger dflags
                (mkMCDiagnostic dflags $ WarningWithFlag Opt_WarnMissedExtraSharedLib)
                  noSrcSpan $ withPprStyle defaultUserStyle (note err)
  where
    dflags = hsc_dflags hsc_env
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


-- ----------------------------------------------------------------------------
-- Loading a dynamic library (dlopen()-ish on Unix, LoadLibrary-ish on Win32)


{- **********************************************************************

                Helper functions

  ********************************************************************* -}

maybePutSDoc :: Logger -> DynFlags -> SDoc -> IO ()
maybePutSDoc logger dflags s
    = when (verbosity dflags > 1) $
          putLogMsg logger dflags
              MCInteractive
              noSrcSpan
              $ withPprStyle defaultUserStyle s

maybePutStr :: Logger -> DynFlags -> String -> IO ()
maybePutStr logger dflags s = maybePutSDoc logger dflags (text s)

maybePutStrLn :: Logger -> DynFlags -> String -> IO ()
maybePutStrLn logger dflags s = maybePutSDoc logger dflags (text s <> text "\n")
