-- | Linking executables
module GHC.Linker.Executable
   ( linkExecutable
   , ExecutableLinkOpts (..)
   , initExecutableLinkOpts
   -- RTS Opts
   , RtsOptsEnabled (..)
   -- * Link info
   , LinkInfo (..)
   , initLinkInfo
   , checkLinkInfo
   , ghcLinkInfoSectionName
   , ghcLinkInfoNoteName
   , platformSupportsSavingLinkOpts
   )
where

import GHC.Prelude
import GHC.Platform
import GHC.Platform.Ways

import GHC.Unit
import GHC.Unit.Env

import GHC.Utils.Asm
import GHC.Utils.Error
import GHC.Utils.Misc
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Logger
import GHC.Utils.TmpFs

import GHC.Driver.Session
import GHC.Driver.Config.Linker

import qualified GHC.Data.ShortText as ST

import GHC.SysTools
import GHC.SysTools.Elf
import GHC.Linker.Config
import GHC.Linker.Unit
import GHC.Linker.MacOS
import GHC.Linker.Windows
import GHC.Linker.Dynamic (libmLinkOpts)
import GHC.Linker.External (runLink)
import GHC.Linker.Static.Utils (exeFileName)

import Control.Monad
import Data.Maybe
import System.FilePath
import System.Directory

data ExecutableLinkOpts = ExecutableLinkOpts
  { leOutputFile :: Maybe FilePath
  , leNameVersion :: GhcNameVersion
  , leWays :: Ways
  , leDynLibLoader :: DynLibLoader
  , leRelativeDynlibPaths :: !Bool
  , leUseXLinkerRPath :: !Bool
  , leSingleLibFolder :: !Bool
  , leWholeArchiveHsLibs :: !Bool
  , leGenManifest :: !Bool
  , leRPath :: !Bool
  , leCompactUnwind :: !Bool
  , leLibraryPaths :: [String]
  , leFrameworkOpts :: FrameworkOpts
  , leManifestOpts :: ManifestOpts
  , leLinkerConfig :: LinkerConfig
  , leOtoolConfig :: OtoolConfig
  , leCcConfig :: CcConfig
  , leInstallNameConfig :: InstallNameConfig
  , leInputs :: [Option]
  , lePieOpts :: [String]
  , leTempDir :: TempDir
  , leVerbFlags :: [String]
  , leNoHsMain :: !Bool
  , leMainSymbol :: String
  , leRtsOptsEnabled :: !RtsOptsEnabled
  , leRtsOptsSuggestions :: !Bool
  , leKeepCafs :: !Bool
  , leRtsOpts :: Maybe String
  }

initExecutableLinkOpts :: DynFlags -> ExecutableLinkOpts
initExecutableLinkOpts dflags =
  let
    platform = targetPlatform dflags
    os = platformOS platform
  in ExecutableLinkOpts
    { leOutputFile = outputFile_ dflags
    , leNameVersion = ghcNameVersion dflags
    , leWays = ways dflags
    , leDynLibLoader = dynLibLoader dflags
    , leRelativeDynlibPaths = gopt Opt_RelativeDynlibPaths dflags
    , leUseXLinkerRPath = useXLinkerRPath dflags os
    , leSingleLibFolder = gopt Opt_SingleLibFolder dflags
    , leWholeArchiveHsLibs = gopt Opt_WholeArchiveHsLibs dflags
    , leGenManifest = gopt Opt_GenManifest dflags
    , leRPath = gopt Opt_RPath dflags
    , leCompactUnwind = gopt Opt_CompactUnwind dflags
    , leLibraryPaths = libraryPaths dflags
    , leFrameworkOpts = initFrameworkOpts dflags
    , leManifestOpts = initManifestOpts dflags
    , leLinkerConfig = initLinkerConfig dflags
    , leCcConfig = configureCc dflags
    , leOtoolConfig = configureOtool dflags
    , leInstallNameConfig = configureInstallName dflags
    , leInputs = ldInputs dflags
    , lePieOpts = pieCCLDOpts dflags
    , leTempDir = tmpDir dflags
    , leVerbFlags = getVerbFlags dflags
    , leNoHsMain = gopt Opt_NoHsMain dflags
    , leMainSymbol = "ZCMain_main"
    , leRtsOptsEnabled = rtsOptsEnabled dflags
    , leRtsOptsSuggestions = rtsOptsSuggestions dflags
    , leKeepCafs = gopt Opt_KeepCAFs dflags
    , leRtsOpts = rtsOpts dflags
    }

leHaveRtsOptsFlags :: ExecutableLinkOpts -> Bool
leHaveRtsOptsFlags opts =
  isJust (leRtsOpts opts)
  || case leRtsOptsEnabled opts of
      RtsOptsSafeOnly -> False
      _ -> True

linkExecutable :: Logger -> TmpFs -> ExecutableLinkOpts -> UnitEnv -> [FilePath] -> [UnitId] -> IO ()
linkExecutable logger tmpfs opts unit_env o_files dep_units = do
    let static_link = False
    let platform   = ue_platform unit_env
        unit_state = ue_homeUnitState unit_env
        verbFlags = leVerbFlags opts
        arch_os   = platformArchOS platform
        output_fn = exeFileName arch_os static_link (leOutputFile opts)
        namever   = leNameVersion opts
        -- For the wasm target, when ghc is invoked with -dynamic,
        -- when linking the final .wasm binary we must still ensure
        -- the static archives are selected. Otherwise wasm-ld would
        -- fail to find and link the .so library dependencies. wasm-ld
        -- can link PIC objects into static .wasm binaries fine, so we
        -- only adjust the ways in the final linking step, and only
        -- when linking .wasm binary (which is supposed to be fully
        -- static), not when linking .so shared libraries.
        ways_
          | ArchWasm32 <- platformArch platform = removeWay WayDyn $ leWays opts
          | otherwise = leWays opts

    full_output_fn <- if isAbsolute output_fn
                      then return output_fn
                      else do d <- getCurrentDirectory
                              return $ normalise (d </> output_fn)

    -- get the full list of packages to link with, by combining the
    -- explicit packages with the auto packages and all of their
    -- dependencies, and eliminating duplicates.
    pkgs <- mayThrowUnitErr (preloadUnitsInfo' unit_env dep_units)
    let pkg_lib_paths     = collectLibraryDirs ways_ pkgs
    let pkg_lib_path_opts = concatMap get_pkg_lib_path_opts pkg_lib_paths
        get_pkg_lib_path_opts l
         | osElfTarget (platformOS platform) &&
           leDynLibLoader opts == SystemDependent &&
           ways_ `hasWay` WayDyn
            = let libpath = if leRelativeDynlibPaths opts
                            then "$ORIGIN" </>
                                 (l `makeRelativeTo` full_output_fn)
                            else l
                  -- See Note [-Xlinker -rpath vs -Wl,-rpath]
                  rpath = if leUseXLinkerRPath opts
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
           leDynLibLoader opts == SystemDependent &&
           ways_ `hasWay` WayDyn &&
           leUseXLinkerRPath opts
            = let libpath = if leRelativeDynlibPaths opts
                            then "@loader_path" </>
                                 (l `makeRelativeTo` full_output_fn)
                            else l
              in ["-L" ++ l] ++ ["-Xlinker", "-rpath", "-Xlinker", libpath]
         | otherwise = ["-L" ++ l]

    pkg_lib_path_opts <-
      if leSingleLibFolder opts
      then do
        libs <- getLibs namever ways_ unit_env dep_units
        tmpDir <- newTempSubDir logger tmpfs (leTempDir opts)
        sequence_ [ copyFile lib (tmpDir </> basename)
                  | (lib, basename) <- libs]
        return [ "-L" ++ tmpDir ]
      else pure pkg_lib_path_opts

    let
      dead_strip
        | leWholeArchiveHsLibs opts = []
        | otherwise = if osSubsectionsViaSymbols (platformOS platform)
                        then ["-Wl,-dead_strip"]
                        else []
    let lib_paths = leLibraryPaths opts
    let lib_path_opts = map ("-L"++) lib_paths

    extraLinkObj <- maybeToList <$> mkExtraObjToLinkIntoBinary logger tmpfs opts unit_state
    noteLinkObjs <- mkNoteObjsToLinkIntoBinary logger tmpfs opts unit_env dep_units

    let
      (pre_hs_libs, post_hs_libs)
        | leWholeArchiveHsLibs opts
        = if platformOS platform == OSDarwin
            then (["-Wl,-all_load"], [])
              -- OS X does not have a flag to turn off -all_load
            else (["-Wl,--whole-archive"], ["-Wl,--no-whole-archive"])
        | otherwise
        = ([],[])

    pkg_link_opts <- do
        unit_link_opts <- getUnitLinkOpts namever ways_ unit_env dep_units
        return $ otherFlags unit_link_opts ++ dead_strip
                  ++ pre_hs_libs ++ hsLibs unit_link_opts ++ post_hs_libs
                  ++ extraLibs unit_link_opts
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
    pkg_framework_opts <- getUnitFrameworkOpts unit_env dep_units
    let framework_opts = getFrameworkOpts (leFrameworkOpts opts) platform

        -- probably _stub.o files
    let extra_ld_inputs = leInputs opts

    rc_objs <- case platformOS platform of
      OSMinGW32 | leGenManifest opts -> maybeCreateManifest logger tmpfs (leManifestOpts opts) output_fn
      _                              -> return []

    let linker_config = leLinkerConfig opts
    let args = ( map GHC.SysTools.Option verbFlags
                 ++ [ GHC.SysTools.Option "-o"
                    , GHC.SysTools.FileOption "" output_fn
                    ]
                 ++ libmLinkOpts platform
                 ++ map GHC.SysTools.Option (
                    []

                 -- See Note [No PIE when linking]
                 ++ lePieOpts opts

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
                 ++ (if not (leCompactUnwind opts) &&
                        linkerSupportsCompactUnwind (leLinkerConfig opts) &&
                        (platformOS platform == OSDarwin) &&
                        case platformArch platform of
                          ArchX86_64  -> True
                          ArchAArch64 -> True
                          _ -> False
                     then ["-Wl,-no_compact_unwind"]
                     else [])

                     -- We should rather be asking does it support --gc-sections?
                 ++ (if linkerIsGnuLd (leLinkerConfig opts) &&
                        not (leWholeArchiveHsLibs opts)
                     then ["-Wl,--gc-sections"]
                     else [])

                 ++ o_files
                 ++ lib_path_opts)
                 ++ extra_ld_inputs
                 ++ map GHC.SysTools.Option (
                    rc_objs
                 ++ framework_opts
                 ++ pkg_lib_path_opts
                 ++ extraLinkObj
                 ++ noteLinkObjs
                 -- See Note [RTS/ghc-internal interface]
                 -- (-u<sym> must come before -lghc-internal...!)
                 ++ (if ghcInternalUnitId `elem` map unitId pkgs
                     then [concat [ "-Wl,-u,"
                                  , ['_' | platformLeadingUnderscore platform]
                                  , "init_ghc_hs_iface" ]]
                     else [])
                 ++ pkg_link_opts
                 ++ pkg_framework_opts
                 ++ (if platformOS platform == OSDarwin
                     --  dead_strip_dylibs, will remove unused dylibs, and thus save
                     --  space in the load commands. The -headerpad is necessary so
                     --  that we can inject more @rpath's later for the left over
                     --  libraries during runInjectRpaths phase.
                     --
                     --  See Note [Dynamic linking on macOS].
                     then [ "-Wl,-dead_strip_dylibs", "-Wl,-headerpad,8000" ]
                     else [])
               ))

    runLink logger tmpfs linker_config args

    -- Make sure to honour -fno-use-rpaths if set on darwin as well; see #20004
    when (platformOS platform == OSDarwin && leRPath opts) $
      GHC.Linker.MacOS.runInjectRPaths logger (leOtoolConfig opts) (leInstallNameConfig opts) pkg_lib_paths output_fn

mkExtraObj :: Logger -> TmpFs -> TempDir -> CcConfig -> UnitState -> Suffix -> String -> IO FilePath
mkExtraObj logger tmpfs tmpdir cc_config unit_state extn xs
 = do
      -- Pass a different set of options to the C compiler depending one whether
      -- we're compiling C or assembler. When compiling C, we pass the usual
      -- set of include directories and PIC flags.
      let cOpts = map Option (ccPicOpts cc_config)
                  ++ map (FileOption "-I" . ST.unpack)
                         (unitIncludeDirs $ unsafeLookupUnit unit_state rtsUnit)
      cFile <- newTempName logger tmpfs tmpdir TFL_CurrentModule extn
      oFile <- newTempName logger tmpfs tmpdir TFL_GhcSession "o"
      writeFile cFile xs
      runCc Nothing logger tmpfs tmpdir cc_config
            ([Option        "-c",
              FileOption "" cFile,
              Option        "-o",
              FileOption "" oFile]
              ++ if extn /= "s"
                    then cOpts
                    else [])
      return oFile

-- | Create object containing main() entry point
--
-- When linking a binary, we need to create a C main() function that
-- starts everything off.  This used to be compiled statically as part
-- of the RTS, but that made it hard to change the -rtsopts setting,
-- so now we generate and compile a main() stub as part of every
-- binary and pass the -rtsopts setting directly to the RTS (#5373)
mkExtraObjToLinkIntoBinary :: Logger -> TmpFs -> ExecutableLinkOpts -> UnitState -> IO (Maybe FilePath)
mkExtraObjToLinkIntoBinary logger tmpfs opts unit_state = do
  when (leNoHsMain opts && leHaveRtsOptsFlags opts) $
     logInfo logger $ withPprStyle defaultUserStyle
         (text "Warning: -rtsopts and -with-rtsopts have no effect with -no-hs-main." $$
          text "    Call hs_init_ghc() from your main() function to set these options.")

  if leNoHsMain opts
    -- Don't try to build the extra object if it is not needed.  Compiling the
    -- extra object assumes the presence of the RTS in the unit database
    -- (because the extra object imports Rts.h) but GHC's build system may try
    -- to build some helper programs before building and registering the RTS!
    -- See #18938 for an example where hp2ps failed to build because of a failed
    -- (unsafe) lookup for the RTS in the unit db.
    then pure Nothing
    else mk_extra_obj exeMain

  where
    tmpdir = leTempDir opts
    cc_config = leCcConfig opts
    mk_extra_obj = fmap Just . mkExtraObj logger tmpfs tmpdir cc_config unit_state "c" . renderWithContext defaultSDocContext

    exeMain = vcat [
        text "#include <Rts.h>",
        text "extern StgClosure " <> text (leMainSymbol opts) <> text "_closure;",
        text "int main(int argc, char *argv[])",
        char '{',
        text " RtsConfig __conf = defaultRtsConfig;",
        text " __conf.rts_opts_enabled = "
            <> text (show (leRtsOptsEnabled opts)) <> semi,
        text " __conf.rts_opts_suggestions = "
            <> (if leRtsOptsSuggestions opts
                then text "true"
                else text "false") <> semi,
        text "__conf.keep_cafs = "
            <> (if leKeepCafs opts
                then text "true"
                else text "false") <> semi,
        case leRtsOpts opts of
            Nothing   -> Outputable.empty
            Just rts_opts -> text "    __conf.rts_opts= " <>
                          text (show rts_opts) <> semi,
        text " __conf.rts_hs_main = true;",
        text " return hs_main(argc,argv,&" <> text (leMainSymbol opts) <> text "_closure,__conf);",
        char '}',
        char '\n' -- final newline, to keep gcc happy
        ]

-- Write out the link info section into a new assembly file. Previously
-- this was included as inline assembly in the main.c file but this
-- is pretty fragile. gas gets upset trying to calculate relative offsets
-- that span the .note section (notably .text) when debug info is present
mkNoteObjsToLinkIntoBinary :: Logger -> TmpFs -> ExecutableLinkOpts -> UnitEnv -> [UnitId] -> IO [FilePath]
mkNoteObjsToLinkIntoBinary logger tmpfs opts unit_env dep_packages = do
   link_info <- initLinkInfo opts unit_env dep_packages

   if (platformSupportsSavingLinkOpts (platformOS platform ))
     then fmap (:[]) $ mkExtraObj logger tmpfs tmpdir cc_config unit_state "s" (renderWithContext defaultSDocContext (link_opts link_info))
     else return []

  where
    unit_state = ue_homeUnitState unit_env
    platform   = ue_platform unit_env
    tmpdir = leTempDir opts
    cc_config = leCcConfig opts
    link_opts info = hcat
        [ -- "link info" section (see Note [LinkInfo section])
          makeElfNote platform ghcLinkInfoSectionName ghcLinkInfoNoteName 0 (show info)

        -- ALL generated assembly must have this section to disable
        -- executable stacks.  See also
        -- "GHC.CmmToAsm" for another instance
        -- where we need to do this.
        , if platformHasGnuNonexecStack platform
            then text ".section .note.GNU-stack,\"\","
                 <> sectionType platform "progbits" <> char '\n'
            else Outputable.empty
        ]

data LinkInfo = LinkInfo
  { liPkgLinkOpts :: UnitLinkOpts
  , liPkgFrameworks :: [String]
  , liRtsOpts :: Maybe String
  , liRtsOptsEnabled :: !RtsOptsEnabled
  , liNoHsMain :: !Bool
  , liLdInputs :: [String]
  , liLdOpts :: [String]
  }
  deriving (Show)


-- | Return the "link info"
--
-- See Note [LinkInfo section]
initLinkInfo :: ExecutableLinkOpts -> UnitEnv -> [UnitId] -> IO LinkInfo
initLinkInfo opts unit_env dep_packages = do
    package_link_opts <- getUnitLinkOpts (leNameVersion opts) (leWays opts) unit_env dep_packages
    pkg_frameworks <- if not (platformUsesFrameworks (ue_platform unit_env))
      then return []
      else do
         ps <- mayThrowUnitErr (preloadUnitsInfo' unit_env dep_packages)
         return (collectFrameworks ps)
    pure $ LinkInfo
      { liPkgLinkOpts = package_link_opts
      , liPkgFrameworks = pkg_frameworks
      , liRtsOpts = leRtsOpts opts
      , liRtsOptsEnabled = leRtsOptsEnabled opts
      , liNoHsMain = leNoHsMain opts
      , liLdInputs = map showOpt (leInputs opts)
      , liLdOpts = map showOpt (linkerOptionsPost (leLinkerConfig opts))
      }

platformSupportsSavingLinkOpts :: OS -> Bool
platformSupportsSavingLinkOpts os
 | os == OSSolaris2 = False -- see #5382
 | otherwise        = osElfTarget os

-- See Note [LinkInfo section]
ghcLinkInfoSectionName :: String
ghcLinkInfoSectionName = ".debug-ghc-link-info"
  -- if we use the ".debug" prefix, then strip will strip it by default

-- Identifier for the note (see Note [LinkInfo section])
ghcLinkInfoNoteName :: String
ghcLinkInfoNoteName = "GHC link info"

-- Returns 'False' if it was, and we can avoid linking, because the
-- previous binary was linked with "the same options".
checkLinkInfo :: Logger -> ExecutableLinkOpts -> UnitEnv -> [UnitId] -> FilePath -> IO Bool
checkLinkInfo logger opts unit_env pkg_deps exe_file
 | not (platformSupportsSavingLinkOpts (platformOS (ue_platform unit_env)))
 -- ToDo: Windows and OS X do not use the ELF binary format, so
 -- readelf does not work there.  We need to find another way to do
 -- this.
 = return False -- conservatively we should return True, but not
                -- linking in this case was the behaviour for a long
                -- time so we leave it as-is.
 | otherwise
 = do
   link_info <- initLinkInfo opts unit_env pkg_deps
   debugTraceMsg logger 3 $ text ("Link info: " ++ show link_info)
   m_exe_link_info <- readElfNoteAsString logger exe_file
                          ghcLinkInfoSectionName ghcLinkInfoNoteName
   let sameLinkInfo = (Just (show link_info) == m_exe_link_info)
   debugTraceMsg logger 3 $ case m_exe_link_info of
     Nothing -> text "Exe link info: Not found"
     Just s
       | sameLinkInfo -> text ("Exe link info is the same")
       | otherwise    -> text ("Exe link info is different: " ++ s)
   return (not sameLinkInfo)

{- Note [LinkInfo section]
   ~~~~~~~~~~~~~~~~~~~~~~~

The "link info" is a string representing the parameters of the link. We save
this information in the binary, and the next time we link, if nothing else has
changed, we use the link info stored in the existing binary to decide whether
to re-link or not.

The "link info" string is stored in a ELF section called ".debug-ghc-link-info"
(see ghcLinkInfoSectionName) with the SHT_NOTE type.  For some time, it used to
not follow the specified record-based format (see #11022).

-}

{-
Note [-Xlinker -rpath vs -Wl,-rpath]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-Wl takes a comma-separated list of options which in the case of
-Wl,-rpath -Wl,some,path,with,commas parses the path with commas
as separate options.
Buck, the build system, produces paths with commas in them.

-Xlinker doesn't have this disadvantage and as far as I can tell
it is supported by both gcc and clang. Anecdotally nvcc supports
-Xlinker, but not -Wl.
-}

