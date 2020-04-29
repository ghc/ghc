{-
-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2001-2003
--
-- Access to system tools: gcc, cp, rm etc
--
-----------------------------------------------------------------------------
-}

{-# LANGUAGE CPP, MultiWayIf, ScopedTypeVariables #-}

module GHC.SysTools (
        -- * Initialisation
        initSysTools,
        lazyInitLlvmConfig,

        -- * Interface to system tools
        module GHC.SysTools.Tasks,
        module GHC.SysTools.Info,

        linkDynLib,

        copy,
        copyWithHeader,

        -- * General utilities
        Option(..),
        expandTopDir,

        -- * Platform-specifics
        libmLinkOpts,

        -- * Mac OS X frameworks
        getPkgFrameworkOpts,
        getFrameworkOpts
 ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Settings.Utils

import GHC.Unit
import GHC.Utils.Outputable
import GHC.Utils.Error
import GHC.Platform
import GHC.Driver.Session
import GHC.Driver.Ways

import Control.Monad.Trans.Except (runExceptT)
import System.FilePath
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO)
import GHC.SysTools.ExtraObj
import GHC.SysTools.Info
import GHC.SysTools.Tasks
import GHC.SysTools.BaseDir
import GHC.Settings.IO
import qualified Data.Set as Set

{-
Note [How GHC finds toolchain utilities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GHC.SysTools.initSysProgs figures out exactly where all the auxiliary programs
are, and initialises mutable variables to make it easy to call them.
To do this, it makes use of definitions in Config.hs, which is a Haskell
file containing variables whose value is figured out by the build system.

Config.hs contains two sorts of things

  cGCC,         The *names* of the programs
  cCPP            e.g.  cGCC = gcc
  cUNLIT                cCPP = gcc -E
  etc           They do *not* include paths


  cUNLIT_DIR   The *path* to the directory containing unlit, split etc
  cSPLIT_DIR   *relative* to the root of the build tree,
                   for use when running *in-place* in a build tree (only)


---------------------------------------------
NOTES for an ALTERNATIVE scheme (i.e *not* what is currently implemented):

Another hair-brained scheme for simplifying the current tool location
nightmare in GHC: Simon originally suggested using another
configuration file along the lines of GCC's specs file - which is fine
except that it means adding code to read yet another configuration
file.  What I didn't notice is that the current package.conf is
general enough to do this:

Package
    {name = "tools",    import_dirs = [],  source_dirs = [],
     library_dirs = [], hs_libraries = [], extra_libraries = [],
     include_dirs = [], c_includes = [],   package_deps = [],
     extra_ghc_opts = ["-pgmc/usr/bin/gcc","-pgml${topdir}/bin/unlit", ... etc.],
     extra_cc_opts = [], extra_ld_opts = []}

Which would have the advantage that we get to collect together in one
place the path-specific package stuff with the path-specific tool
stuff.
                End of NOTES
---------------------------------------------

************************************************************************
*                                                                      *
\subsection{Initialisation}
*                                                                      *
************************************************************************
-}

-- Note [LLVM configuration]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The `llvm-targets` and `llvm-passes` files are shipped with GHC and contain
-- information needed by the LLVM backend to invoke `llc` and `opt`.
-- Specifically:
--
--  * llvm-targets maps autoconf host triples to the corresponding LLVM
--    `data-layout` declarations. This information is extracted from clang using
--    the script in utils/llvm-targets/gen-data-layout.sh and should be updated
--    whenever we target a new version of LLVM.
--
--  * llvm-passes maps GHC optimization levels to sets of LLVM optimization
--    flags that GHC should pass to `opt`.
--
-- This information is contained in files rather the GHC source to allow users
-- to add new targets to GHC without having to recompile the compiler.
--
-- Since this information is only needed by the LLVM backend we load it lazily
-- with unsafeInterleaveIO. Consequently it is important that we lazily pattern
-- match on LlvmConfig until we actually need its contents.

lazyInitLlvmConfig :: String
               -> IO LlvmConfig
lazyInitLlvmConfig top_dir
  = unsafeInterleaveIO $ do    -- see Note [LLVM configuration]
      targets <- readAndParse "llvm-targets" mkLlvmTarget
      passes <- readAndParse "llvm-passes" id
      return $ LlvmConfig { llvmTargets = targets, llvmPasses = passes }
  where
    readAndParse name builder =
      do let llvmConfigFile = top_dir </> name
         llvmConfigStr <- readFile llvmConfigFile
         case maybeReadFuzzy llvmConfigStr of
           Just s -> return (fmap builder <$> s)
           Nothing -> pgmError ("Can't parse " ++ show llvmConfigFile)

    mkLlvmTarget :: (String, String, String) -> LlvmTarget
    mkLlvmTarget (dl, cpu, attrs) = LlvmTarget dl cpu (words attrs)


initSysTools :: String          -- TopDir path
             -> IO Settings     -- Set all the mutable variables above, holding
                                --      (a) the system programs
                                --      (b) the package-config file
                                --      (c) the GHC usage message
initSysTools top_dir = do
  res <- runExceptT $ initSettings top_dir
  case res of
    Right a -> pure a
    Left (SettingsError_MissingData msg) -> pgmError msg
    Left (SettingsError_BadData msg) -> pgmError msg

{- Note [Windows stack usage]

See: #8870 (and #8834 for related info) and #12186

On Windows, occasionally we need to grow the stack. In order to do
this, we would normally just bump the stack pointer - but there's a
catch on Windows.

If the stack pointer is bumped by more than a single page, then the
pages between the initial pointer and the resulting location must be
properly committed by the Windows virtual memory subsystem. This is
only needed in the event we bump by more than one page (i.e 4097 bytes
or more).

Windows compilers solve this by emitting a call to a special function
called _chkstk, which does this committing of the pages for you.

The reason this was causing a segfault was because due to the fact the
new code generator tends to generate larger functions, we needed more
stack space in GHC itself. In the x86 codegen, we needed approximately
~12kb of stack space in one go, which caused the process to segfault,
as the intervening pages were not committed.

GCC can emit such a check for us automatically but only when the flag
-fstack-check is used.

See https://gcc.gnu.org/onlinedocs/gnat_ugn/Stack-Overflow-Checking.html
for more information.

-}

copy :: DynFlags -> String -> FilePath -> FilePath -> IO ()
copy dflags purpose from to = copyWithHeader dflags purpose Nothing from to

copyWithHeader :: DynFlags -> String -> Maybe String -> FilePath -> FilePath
               -> IO ()
copyWithHeader dflags purpose maybe_header from to = do
  showPass dflags purpose

  hout <- openBinaryFile to   WriteMode
  hin  <- openBinaryFile from ReadMode
  ls <- hGetContents hin -- inefficient, but it'll do for now. ToDo: speed up
  maybe (return ()) (header hout) maybe_header
  hPutStr hout ls
  hClose hout
  hClose hin
 where
  -- write the header string in UTF-8.  The header is something like
  --   {-# LINE "foo.hs" #-}
  -- and we want to make sure a Unicode filename isn't mangled.
  header h str = do
   hSetEncoding h utf8
   hPutStr h str
   hSetBinaryMode h True

{-
************************************************************************
*                                                                      *
\subsection{Support code}
*                                                                      *
************************************************************************
-}

linkDynLib :: DynFlags -> [String] -> [UnitId] -> IO ()
linkDynLib dflags0 o_files dep_packages
 = do
    let -- This is a rather ugly hack to fix dynamically linked
        -- GHC on Windows. If GHC is linked with -threaded, then
        -- it links against libHSrts_thr. But if base is linked
        -- against libHSrts, then both end up getting loaded,
        -- and things go wrong. We therefore link the libraries
        -- with the same RTS flags that we link GHC with.
        dflags1 = if platformMisc_ghcThreaded $ platformMisc dflags0
          then addWay' WayThreaded dflags0
          else                     dflags0
        dflags2 = if platformMisc_ghcDebugged $ platformMisc dflags1
          then addWay' WayDebug dflags1
          else                  dflags1
        dflags = updateWays dflags2

        verbFlags = getVerbFlags dflags
        o_file = outputFile dflags

    pkgs <- getPreloadPackagesAnd dflags dep_packages

    let pkg_lib_paths = collectLibraryPaths dflags pkgs
    let pkg_lib_path_opts = concatMap get_pkg_lib_path_opts pkg_lib_paths
        get_pkg_lib_path_opts l
         | ( osElfTarget (platformOS (targetPlatform dflags)) ||
             osMachOTarget (platformOS (targetPlatform dflags)) ) &&
           dynLibLoader dflags == SystemDependent &&
           WayDyn `Set.member` ways dflags
            = ["-L" ++ l, "-Xlinker", "-rpath", "-Xlinker", l]
              -- See Note [-Xlinker -rpath vs -Wl,-rpath]
         | otherwise = ["-L" ++ l]

    let lib_paths = libraryPaths dflags
    let lib_path_opts = map ("-L"++) lib_paths

    -- We don't want to link our dynamic libs against the RTS package,
    -- because the RTS lib comes in several flavours and we want to be
    -- able to pick the flavour when a binary is linked.
    -- On Windows we need to link the RTS import lib as Windows does
    -- not allow undefined symbols.
    -- The RTS library path is still added to the library search path
    -- above in case the RTS is being explicitly linked in (see #3807).
    let platform = targetPlatform dflags
        os = platformOS platform
        pkgs_no_rts = case os of
                      OSMinGW32 ->
                          pkgs
                      _ ->
                          filter ((/= rtsUnitId) . mkUnit) pkgs
    let pkg_link_opts = let (package_hs_libs, extra_libs, other_flags) = collectLinkOpts dflags pkgs_no_rts
                        in  package_hs_libs ++ extra_libs ++ other_flags

        -- probably _stub.o files
        -- and last temporary shared object file
    let extra_ld_inputs = ldInputs dflags

    -- frameworks
    pkg_framework_opts <- getPkgFrameworkOpts dflags platform
                                              (map unitId pkgs)
    let framework_opts = getFrameworkOpts dflags platform

    case os of
        OSMinGW32 -> do
            -------------------------------------------------------------
            -- Making a DLL
            -------------------------------------------------------------
            let output_fn = case o_file of
                            Just s -> s
                            Nothing -> "HSdll.dll"

            runLink dflags (
                    map Option verbFlags
                 ++ [ Option "-o"
                    , FileOption "" output_fn
                    , Option "-shared"
                    ] ++
                    [ FileOption "-Wl,--out-implib=" (output_fn ++ ".a")
                    | gopt Opt_SharedImplib dflags
                    ]
                 ++ map (FileOption "") o_files

                 -- Permit the linker to auto link _symbol to _imp_symbol
                 -- This lets us link against DLLs without needing an "import library"
                 ++ [Option "-Wl,--enable-auto-import"]

                 ++ extra_ld_inputs
                 ++ map Option (
                    lib_path_opts
                 ++ pkg_lib_path_opts
                 ++ pkg_link_opts
                ))
        _ | os == OSDarwin -> do
            -------------------------------------------------------------------
            -- Making a darwin dylib
            -------------------------------------------------------------------
            -- About the options used for Darwin:
            -- -dynamiclib
            --   Apple's way of saying -shared
            -- -undefined dynamic_lookup:
            --   Without these options, we'd have to specify the correct
            --   dependencies for each of the dylibs. Note that we could
            --   (and should) do without this for all libraries except
            --   the RTS; all we need to do is to pass the correct
            --   HSfoo_dyn.dylib files to the link command.
            --   This feature requires Mac OS X 10.3 or later; there is
            --   a similar feature, -flat_namespace -undefined suppress,
            --   which works on earlier versions, but it has other
            --   disadvantages.
            -- -single_module
            --   Build the dynamic library as a single "module", i.e. no
            --   dynamic binding nonsense when referring to symbols from
            --   within the library. The NCG assumes that this option is
            --   specified (on i386, at least).
            -- -install_name
            --   Mac OS/X stores the path where a dynamic library is (to
            --   be) installed in the library itself.  It's called the
            --   "install name" of the library. Then any library or
            --   executable that links against it before it's installed
            --   will search for it in its ultimate install location.
            --   By default we set the install name to the absolute path
            --   at build time, but it can be overridden by the
            --   -dylib-install-name option passed to ghc. Cabal does
            --   this.
            -------------------------------------------------------------------

            let output_fn = case o_file of { Just s -> s; Nothing -> "a.out"; }

            instName <- case dylibInstallName dflags of
                Just n -> return n
                Nothing -> return $ "@rpath" `combine` (takeFileName output_fn)
            runLink dflags (
                    map Option verbFlags
                 ++ [ Option "-dynamiclib"
                    , Option "-o"
                    , FileOption "" output_fn
                    ]
                 ++ map Option o_files
                 ++ [ Option "-undefined",
                      Option "dynamic_lookup",
                      Option "-single_module" ]
                 ++ (if platformArch platform == ArchX86_64
                     then [ ]
                     else [ Option "-Wl,-read_only_relocs,suppress" ])
                 ++ [ Option "-install_name", Option instName ]
                 ++ map Option lib_path_opts
                 ++ extra_ld_inputs
                 ++ map Option framework_opts
                 ++ map Option pkg_lib_path_opts
                 ++ map Option pkg_link_opts
                 ++ map Option pkg_framework_opts
                 ++ [ Option "-Wl,-dead_strip_dylibs" ]
              )
        _ -> do
            -------------------------------------------------------------------
            -- Making a DSO
            -------------------------------------------------------------------

            let output_fn = case o_file of { Just s -> s; Nothing -> "a.out"; }
                unregisterised = platformUnregisterised (targetPlatform dflags)
            let bsymbolicFlag = -- we need symbolic linking to resolve
                                -- non-PIC intra-package-relocations for
                                -- performance (where symbolic linking works)
                                -- See Note [-Bsymbolic assumptions by GHC]
                                ["-Wl,-Bsymbolic" | not unregisterised]

            runLink dflags (
                    map Option verbFlags
                 ++ libmLinkOpts
                 ++ [ Option "-o"
                    , FileOption "" output_fn
                    ]
                 ++ map Option o_files
                 ++ [ Option "-shared" ]
                 ++ map Option bsymbolicFlag
                    -- Set the library soname. We use -h rather than -soname as
                    -- Solaris 10 doesn't support the latter:
                 ++ [ Option ("-Wl,-h," ++ takeFileName output_fn) ]
                 ++ extra_ld_inputs
                 ++ map Option lib_path_opts
                 ++ map Option pkg_lib_path_opts
                 ++ map Option pkg_link_opts
              )

-- | Some platforms require that we explicitly link against @libm@ if any
-- math-y things are used (which we assume to include all programs). See #14022.
libmLinkOpts :: [Option]
libmLinkOpts =
#if defined(HAVE_LIBM)
  [Option "-lm"]
#else
  []
#endif

getPkgFrameworkOpts :: DynFlags -> Platform -> [UnitId] -> IO [String]
getPkgFrameworkOpts dflags platform dep_packages
  | platformUsesFrameworks platform = do
    pkg_framework_path_opts <- do
        pkg_framework_paths <- getPackageFrameworkPath dflags dep_packages
        return $ map ("-F" ++) pkg_framework_paths

    pkg_framework_opts <- do
        pkg_frameworks <- getPackageFrameworks dflags dep_packages
        return $ concat [ ["-framework", fw] | fw <- pkg_frameworks ]

    return (pkg_framework_path_opts ++ pkg_framework_opts)

  | otherwise = return []

getFrameworkOpts :: DynFlags -> Platform -> [String]
getFrameworkOpts dflags platform
  | platformUsesFrameworks platform = framework_path_opts ++ framework_opts
  | otherwise = []
  where
    framework_paths     = frameworkPaths dflags
    framework_path_opts = map ("-F" ++) framework_paths

    frameworks     = cmdlineFrameworks dflags
    -- reverse because they're added in reverse order from the cmd line:
    framework_opts = concat [ ["-framework", fw]
                            | fw <- reverse frameworks ]

{-
Note [-Bsymbolic assumptions by GHC]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GHC has a few assumptions about interaction of relocations in NCG and linker:

1. -Bsymbolic resolves internal references when the shared library is linked,
   which is important for performance.
2. When there is a reference to data in a shared library from the main program,
   the runtime linker relocates the data object into the main program using an
   R_*_COPY relocation.
3. If we used -Bsymbolic, then this results in multiple copies of the data
   object, because some references have already been resolved to point to the
   original instance. This is bad!

We work around [3.] for native compiled code by avoiding the generation of
R_*_COPY relocations.

Unregisterised compiler can't evade R_*_COPY relocations easily thus we disable
-Bsymbolic linking there.

See related tickets: #4210, #15338
-}
