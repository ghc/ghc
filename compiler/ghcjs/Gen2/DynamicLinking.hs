{-# LANGUAGE CPP, NondecreasingIndentation, TupleSections #-}
{-
  Various utilities for building and loading dynamic libraries, to make Template Haskell
  work in GHCJS
 -}

module Gen2.DynamicLinking ( ghcjsLink
                           , ghcjsDoLink
                           -- , isGhcjsPrimPackage
                           -- , ghcjsPrimPackage
                           ) where

import Outputable hiding ((<>))
import FastString
import HscTypes
import Util
import Exception
import Packages
import DynFlags
import Module
import SrcLoc
import BasicTypes
import SysTools.ExtraObj
import GHC.Platform
import ErrUtils
import DriverPhases
import DriverPipeline hiding ( linkingNeeded )
import UniqDFM
import Maybes hiding ( Succeeded )
import Prelude

import           Control.Monad

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import           Data.Either
import           Data.List ( nub )
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T

import           System.Directory
import           System.FilePath

import           Compiler.Compat
import           Compiler.Settings
import           Compiler.Variants
import qualified Compiler.Utils as Utils
import qualified Compiler.Info as Info

import           SysTools

-- import qualified Data.Yaml                as Yaml
import Compiler.Info (getLibDir)
import Gen2.Archive


-------------------------------------------------------------------------------------------
-- Link libraries

ghcjsLink :: GhcjsEnv
          -> GhcjsSettings
          -> [FilePath] -- ^ extra JS files
          -> Bool       -- ^ build JavaScript?
          -> GhcLink    -- ^ what to link
          -> DynFlags
          -> Bool
          -> HomePackageTable
          -> IO SuccessFlag
ghcjsLink env settings extraJs buildJs ghcLink dflags batch_attempt_linking pt
  | ghcLink == LinkInMemory || ghcLink == NoLink =
      return Succeeded
  | ghcLink == LinkStaticLib || ghcLink == LinkDynLib =
      if buildJs && isJust (gsLinkJsLib settings)
         then ghcjsLinkJsLib settings extraJs dflags pt
         else return Succeeded
  | otherwise = do
      when (buildJs && isJust (gsLinkJsLib settings))
        (void $ ghcjsLinkJsLib settings extraJs dflags pt) -- fixme use return value
      link' env settings extraJs buildJs dflags batch_attempt_linking pt

ghcjsLinkJsLib :: GhcjsSettings
               -> [FilePath] -- ^ extra JS files
               -> DynFlags
               -> HomePackageTable
               -> IO SuccessFlag
ghcjsLinkJsLib settings jsFiles dflags hpt
  | Just jsLib <- gsLinkJsLib settings = do
      let profSuff | WayProf `elem` ways dflags = "_p"
                   | otherwise                  = ""
          libFileName    = ("lib" ++ jsLib ++ profSuff) <.> "js_a"
          inOutputDir file =
            maybe file
                  (</>file)
                  (gsJsLibOutputDir settings `mplus` objectDir dflags)
          outputFile     = inOutputDir libFileName
          jsFiles' = nub (gsJsLibSrcs settings ++ jsFiles)
          meta    = Meta (opt_P dflags)
      jsEntries <- forM jsFiles' $ \file ->
        (JsSource file,) . B.fromStrict <$> BS.readFile file
      objEntries <- forM (eltsUDFM hpt) $ \hmi -> do
        let mt    = T.pack . moduleNameString . moduleName . mi_module . hm_iface $ hmi
            files = maybe [] (\l -> [ o | DotO o <- linkableUnlinked l]) (hm_linkable hmi)
        -- fixme archive does not handle multiple files for a module yet
        forM files $ \file ->
          (Object mt,) . B.fromStrict <$> BS.readFile file
      B.writeFile outputFile (buildArchive meta (concat objEntries ++ jsEntries))
      -- we don't use shared js_so libraries ourselves, but Cabal expects that we
      -- generate one when building with --dynamic-too. Just write an empty file
      when (gopt Opt_BuildDynamicToo dflags || WayDyn `elem` ways dflags) $ do
        let sharedLibFileName =
              "lib" ++ jsLib ++ "-ghcjs" ++ Info.getCompilerVersion ++ profSuff <.> "js_so"
            sharedOutputFile = inOutputDir sharedLibFileName
        -- keep strip happy
        BS.writeFile sharedOutputFile =<< BS.readFile (topDir dflags </> "empty.o")
      return Succeeded
  | otherwise =
      return Succeeded

ghcjsLinkJsBinary :: GhcjsEnv
                  -> GhcjsSettings
                  -> [FilePath]
                  -> DynFlags
                  -> [FilePath]
                  -> [InstalledUnitId]
                  -> IO ()
ghcjsLinkJsBinary env settings jsFiles dflags objs dep_pkgs =
  void $ variantLink gen2Variant dflags env settings exe [] dep_pkgs objs' jsFiles isRoot S.empty
    where
      objs'    = map ObjFile objs
      isRoot _ = True
      exe      = Utils.exeFileName dflags
      packageLibPaths :: InstalledUnitId -> [FilePath]
      packageLibPaths = maybe [] libraryDirs . lookupInstalledPackage dflags

{-
isGhcjsPrimPackage :: DynFlags -> InstalledUnitId -> Bool
isGhcjsPrimPackage dflags pkgKey
  =  getInstalledPackageName dflags pkgKey == "ghcjs-prim" ||
     (pkgKey == thisInstalledUnitId dflags &&
      elem "-DBOOTING_PACKAGE=ghcjs-prim" (opt_P dflags))

ghcjsPrimPackage :: DynFlags -> IO InstalledUnitId
ghcjsPrimPackage dflags = do
  keys <- BS.readFile filename
  case Yaml.decodeEither keys of
    Left _err -> error $ "could not read wired-in package keys from " ++ filename
    Right m -> case M.lookup "ghcjs-prim" m of
      Nothing -> error "Package `ghcjs-prim' is required to link executables"
      Just k -> return (stringToPackageKey k)
  where
    filename = getLibDir dflags </> "wiredinkeys" <.> "yaml"
-}

link' :: GhcjsEnv
      -> GhcjsSettings
      -> [FilePath]              -- extra js files
      -> Bool                    -- building JavaScript
      -> DynFlags                -- dynamic flags
      -> Bool                    -- attempt linking in batch mode?
      -> HomePackageTable        -- what to link
      -> IO SuccessFlag

link' env settings extraJs buildJs dflags batch_attempt_linking hpt
   | batch_attempt_linking
   = do
        let
            staticLink = case ghcLink dflags of
                          LinkStaticLib -> True
                          _ -> False

            home_mod_infos = eltsUDFM hpt

            -- the packages we depend on
            pkg_deps  = concatMap (map fst . dep_pkgs . mi_deps . hm_iface) home_mod_infos

            -- the linkables to link
            linkables = map (expectJust "link".hm_linkable) home_mod_infos
        debugTraceMsg dflags 3 (text "link: pkgdeps ..." $$ vcat (map ppr pkg_deps))
        debugTraceMsg dflags 3 (text "link: linkables are ..." $$ vcat (map ppr linkables))

        -- check for the -no-link flag
        if isNoLink (ghcLink dflags)
          then do debugTraceMsg dflags 3 (text "link(batch): linking omitted (-c flag given).")
                  return Succeeded
          else do

        let getOfiles (LM _ _ us) = map nameOfObject (filter isObject us)
            obj_files = concatMap getOfiles linkables

            exe_file = exeFileName staticLink dflags

        linking_needed <- linkingNeeded dflags staticLink linkables pkg_deps

        if not (gopt Opt_ForceRecomp dflags) && not linking_needed
           then do debugTraceMsg dflags 2 (text exe_file <+> ptext (sLit "is up to date, linking not required."))
                   return Succeeded
           else do

        unless buildJs $
           compilationProgressMsg dflags ("Linking " ++ exe_file ++ " ...")

        -- Don't showPass in Batch mode; doLink will do that for us.
        let link = case ghcLink dflags of
                LinkBinary    -> if buildJs
                                 then ghcjsLinkJsBinary env settings extraJs
                                 else linkBinary
                LinkStaticLib -> linkStaticLib
                LinkDynLib    -> linkDynLibCheck
                other         -> panicBadLink other

        -- make sure we link ghcjs-prim even when it's not a dependency
        -- pkg_deps' <- if any (isGhcjsPrimPackage dflags) pkg_deps
        --                then return pkg_deps
        --                else (:pkg_deps) <$> ghcjsPrimPackage dflags

        link dflags obj_files pkg_deps

        debugTraceMsg dflags 3 (text "link: done")

        -- linkBinary only returns if it succeeds
        return Succeeded

   | otherwise
   = do debugTraceMsg dflags 3 (text "link(batch): upsweep (partially) failed OR" $$
                                text "   Main.main not exported; not linking.")
        return Succeeded


linkingNeeded :: DynFlags -> Bool -> [Linkable] -> [InstalledUnitId] -> IO Bool
linkingNeeded dflags staticLink linkables pkg_deps = do
        -- if the modification time on the executable is later than the
        -- modification times on all of the objects and libraries, then omit
        -- linking (unless the -fforce-recomp flag was given).
  let exe_file = exeFileName staticLink dflags
  e_exe_time <- tryIO $ getModificationUTCTime exe_file
  case e_exe_time of
    Left _  -> return True
    Right t -> do
        -- first check object files and extra_ld_inputs
        let extra_ld_inputs = [ f | FileOption _ f <- ldInputs dflags ]
        e_extra_times <- mapM (tryIO . getModificationUTCTime) extra_ld_inputs
        let (errs,extra_times) = partitionEithers e_extra_times
        let obj_times =  map linkableTime linkables ++ extra_times
        if not (null errs) || any (t <) obj_times
            then return True
            else do

        -- next, check libraries. XXX this only checks Haskell libraries,
        -- not extra_libraries or -l things from the command line.
        let pkg_hslibs  = [ (libraryDirs c, lib)
                          | Just c <- map (lookupInstalledPackage dflags) pkg_deps,
                            lib <- packageHsLibs dflags c ]

        pkg_libfiles <- mapM (uncurry (findHSLib dflags)) pkg_hslibs
        if any isNothing pkg_libfiles then return True else do
        e_lib_times <- mapM (tryIO . getModificationUTCTime)
                          (catMaybes pkg_libfiles)
        let (lib_errs,lib_times) = partitionEithers e_lib_times
        if not (null lib_errs) || any (t <) lib_times
           then return True
           else checkLinkInfo dflags pkg_deps exe_file

panicBadLink :: GhcLink -> a
panicBadLink other = panic ("link: GHC not built to link this way: " ++
                            show other)

linkDynLibCheck :: DynFlags -> [String] -> [InstalledUnitId] -> IO ()
linkDynLibCheck dflags o_files dep_packages
 = do
    when (haveRtsOptsFlags dflags) $
      log_action dflags dflags NoReason SevInfo noSrcSpan (defaultUserStyle dflags)
          (text "Warning: -rtsopts and -with-rtsopts have no effect with -shared." $$
           text "    Call hs_init_ghc() from your main() function to set these options.")

    linkDynLib dflags o_files dep_packages

linkStaticLib :: DynFlags -> [String] -> [InstalledUnitId] -> IO ()
linkStaticLib dflags o_files dep_packages
  = -- XXX looks like this needs to be updated
{-
 = do
    when (platformOS (targetPlatform dflags) `notElem` [OSiOS, OSDarwin]) $
      throwGhcExceptionIO (ProgramError "Static archive creation only supported on Darwin/OS X/iOS")
-}
    linkBinary' True dflags o_files dep_packages

findHSLib :: DynFlags -> [String] -> String -> IO (Maybe FilePath)
findHSLib dflags dirs lib = do
  let batch_lib_file = if ghcLink dflags == LinkStaticLib
                       then "lib" ++ lib <.> "a"
                       else mkSOName (targetPlatform dflags) lib
  found <- filterM doesFileExist (map (</> batch_lib_file) dirs)
  case found of
    [] -> return Nothing
    (x:_) -> return (Just x)

linkBinary' :: Bool -> DynFlags -> [FilePath] -> [InstalledUnitId] -> IO ()
linkBinary' staticLink dflags o_files dep_packages = do
    let platform = targetPlatform dflags
        mySettings = settings dflags
        verbFlags = getVerbFlags dflags
        output_fn = exeFileName staticLink dflags

    -- get the full list of packages to link with, by combining the
    -- explicit packages with the auto packages and all of their
    -- dependencies, and eliminating duplicates.

    full_output_fn <- if isAbsolute output_fn
                      then return output_fn
                      else do d <- getCurrentDirectory
                              return $ normalise (d </> output_fn)
    pkg_lib_paths <- getPackageLibraryPath dflags dep_packages
    let pkg_lib_path_opts = concatMap get_pkg_lib_path_opts pkg_lib_paths
        get_pkg_lib_path_opts l
         | osElfTarget (platformOS platform) &&
           dynLibLoader dflags == SystemDependent &&
           WayDyn `elem` ways dflags
            = let libpath = if gopt Opt_RelativeDynlibPaths dflags
                            then "$ORIGIN" </>
                                 (l `makeRelativeTo` full_output_fn)
                            else l
                  -- See Note [-Xlinker -rpath vs -Wl,-rpath]
                  rpath = if gopt Opt_RPath dflags
                          then ["-Xlinker", "-rpath", "-Xlinker", libpath]
                          else []
                  -- Solaris 11's linker does not support -rpath-link option. It silently
                  -- ignores it and then complains about next option which is -l<some
                  -- dir> as being a directory and not expected object file, E.g
                  -- ld: elf error: file
                  -- /tmp/ghc-src/libraries/base/dist-install/build:
                  -- elf_begin: I/O error: region read: Is a directory
                  rpathlink = if (platformOS platform) == OSSolaris2
                              then []
                              else ["-Xlinker", "-rpath-link", "-Xlinker", l]
              in ["-L" ++ l] ++ rpathlink ++ rpath
         | osMachOTarget (platformOS platform) &&
           dynLibLoader dflags == SystemDependent &&
           WayDyn `elem` ways dflags &&
           gopt Opt_RPath dflags
            = let libpath = if gopt Opt_RelativeDynlibPaths dflags
                            then "@loader_path" </>
                                 (l `makeRelativeTo` full_output_fn)
                            else l
              in ["-L" ++ l] ++ ["-Xlinker", "-rpath", "-Xlinker", libpath]
         | otherwise = ["-L" ++ l]

    let
      dead_strip
        | gopt Opt_WholeArchiveHsLibs dflags = []
        | otherwise = if osSubsectionsViaSymbols (platformOS platform)
                      then ["-Wl,-dead_strip"]
                      else []
    let lib_paths = libraryPaths dflags
    let lib_path_opts = map ("-L"++) lib_paths

    extraLinkObj <- mkExtraObjToLinkIntoBinary dflags
    noteLinkObjs <- mkNoteObjsToLinkIntoBinary dflags dep_packages

    let
      (pre_hs_libs, post_hs_libs)
        | gopt Opt_WholeArchiveHsLibs dflags
        = if platformOS platform == OSDarwin
            then (["-Wl,-all_load"], [])
              -- OS X does not have a flag to turn off -all_load
            else (["-Wl,--whole-archive"], ["-Wl,--no-whole-archive"])
        | otherwise
        = ([],[])

    pkg_link_opts <- do
        (package_hs_libs, extra_libs, other_flags) <- getPackageLinkOpts dflags dep_packages
        return $ if staticLink
            then package_hs_libs -- If building an executable really means making a static
                                 -- library (e.g. iOS), then we only keep the -l options for
                                 -- HS packages, because libtool doesn't accept other options.
                                 -- In the case of iOS these need to be added by hand to the
                                 -- final link in Xcode.
            else other_flags ++ dead_strip
                  ++ pre_hs_libs ++ package_hs_libs ++ post_hs_libs
                  ++ extra_libs
                 -- -Wl,-u,<sym> contained in other_flags
                 -- needs to be put before -l<package>,
                 -- otherwise Solaris linker fails linking
                 -- a binary with unresolved symbols in RTS
                 -- which are defined in base package
                 -- the reason for this is a note in ld(1) about
                 -- '-u' option: "The placement of this option
                 -- on the command line is significant.
                 -- This option must be placed before the library
                 -- that defines the symbol."

    -- frameworks
    pkg_framework_opts <- getPkgFrameworkOpts dflags platform dep_packages
    let framework_opts = getFrameworkOpts dflags platform

        -- probably _stub.o files
    let extra_ld_inputs = ldInputs dflags

    -- Here are some libs that need to be linked at the *end* of
    -- the command line, because they contain symbols that are referred to
    -- by the RTS.  We can't therefore use the ordinary way opts for these.
    let
        debug_opts | WayDebug `elem` ways dflags = [
#if defined(HAVE_LIBBFD)
                        "-lbfd", "-liberty"
#endif
                         ]
                   | otherwise            = []

        thread_opts | WayThreaded `elem` ways dflags = [
#if NEED_PTHREAD_LIB
                        "-lpthread"
#endif
                        ]
                    | otherwise               = []

    rc_objs <- maybeCreateManifest dflags output_fn

    let link = if staticLink
                   then SysTools.runLibtool
                   else SysTools.runLink
    link dflags (
                       map SysTools.Option verbFlags
                      ++ [ SysTools.Option "-o"
                         , SysTools.FileOption "" output_fn
                         ]
                      ++ map SysTools.Option (
                         []

                      -- See Note [No PIE when linking]
                      ++ picCCOpts dflags

                      -- Permit the linker to auto link _symbol to _imp_symbol.
                      -- This lets us link against DLLs without needing an "import library".
                      ++ (if platformOS platform == OSMinGW32
                          then ["-Wl,--enable-auto-import"]
                          else [])

                      -- '-no_compact_unwind'
                      -- C++/Objective-C exceptions cannot use optimised
                      -- stack unwinding code. The optimised form is the
                      -- default in Xcode 4 on at least x86_64, and
                      -- without this flag we're also seeing warnings
                      -- like
                      --     ld: warning: could not create compact unwind for .LFB3: non-standard register 5 being saved in prolog
                      -- on x86.
                      ++ (if sLdSupportsCompactUnwind mySettings &&
                             not staticLink &&
                             (platformOS platform == OSDarwin) &&
                             case platformArch platform of
                               ArchX86 -> True
                               ArchX86_64 -> True
                               ArchARM {} -> True
                               ArchAArch64    -> True
                               _ -> False
                          then ["-Wl,-no_compact_unwind"]
                          else [])

                      -- '-Wl,-read_only_relocs,suppress'
                      -- ld gives loads of warnings like:
                      --     ld: warning: text reloc in _base_GHCziArr_unsafeArray_info to _base_GHCziArr_unsafeArray_closure
                      -- when linking any program. We're not sure
                      -- whether this is something we ought to fix, but
                      -- for now this flags silences them.
                      ++ (if platformOS   platform == OSDarwin &&
                             platformArch platform == ArchX86 &&
                             not staticLink
                          then ["-Wl,-read_only_relocs,suppress"]
                          else [])

                      ++ (if sLdIsGnuLd mySettings &&
                             not (gopt Opt_WholeArchiveHsLibs dflags)
                          then ["-Wl,--gc-sections"]
                          else [])

                      ++ o_files
                      ++ lib_path_opts)
                      ++ extra_ld_inputs
                      ++ map SysTools.Option (
                         rc_objs
                      ++ framework_opts
                      ++ pkg_lib_path_opts
                      ++ extraLinkObj:noteLinkObjs
                      ++ pkg_link_opts
                      ++ pkg_framework_opts
                      ++ debug_opts
                      ++ thread_opts
                    ))


ghcjsDoLink :: GhcjsEnv -> GhcjsSettings -> Bool -> DynFlags -> Phase -> [FilePath] -> IO ()
ghcjsDoLink env settings native dflags stop_phase o_files
  | not (isStopLn stop_phase)
  = return ()           -- We stopped before the linking phase
  | native
  = case ghcLink dflags of
        NoLink     -> return ()
        LinkBinary -> linkBinary      dflags o_files []
        LinkDynLib -> linkDynLibCheck dflags o_files []
        other      -> panicBadLink other
  | isJust (gsLinkJsLib settings)
  = void $ ghcjsLinkJsLib settings o_files dflags emptyHomePackageTable
  | otherwise =
    void $ ghcjsLink env settings o_files True (ghcLink dflags) dflags True emptyHomePackageTable

