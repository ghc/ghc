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

module SysTools (
        -- Initialisation
        initSysTools,
        initLlvmTargets,

        -- Interface to system tools
        module SysTools.Tasks,
        module SysTools.Info,

        linkDynLib,

        copy,
        copyWithHeader,

        Option(..),

        -- platform-specifics
        libmLinkOpts,

        -- frameworks
        getPkgFrameworkOpts,
        getFrameworkOpts
 ) where

#include "HsVersions.h"

import GhcPrelude

import Module
import Packages
import Config
import Outputable
import ErrUtils
import Panic
import Platform
import Util
import DynFlags

import System.FilePath
import System.IO
import System.Directory
import SysTools.ExtraObj
import SysTools.Info
import SysTools.Tasks
import Data.List

#if defined(mingw32_HOST_OS)
#if MIN_VERSION_Win32(2,5,0)
import qualified System.Win32.Types as Win32
#else
import qualified System.Win32.Info as Win32
#endif
import Data.Char
import Exception
import Foreign
import Foreign.C.String
import System.Win32.Types (DWORD, LPTSTR, HANDLE)
import System.Win32.Types (failIfNull, failIf, iNVALID_HANDLE_VALUE)
import System.Win32.File (createFile,closeHandle, gENERIC_READ, fILE_SHARE_READ, oPEN_EXISTING, fILE_ATTRIBUTE_NORMAL, fILE_FLAG_BACKUP_SEMANTICS )
import System.Win32.DLL (loadLibrary, getProcAddress)
#endif

#if defined(mingw32_HOST_OS)
# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif
#endif

{-
How GHC finds its files
~~~~~~~~~~~~~~~~~~~~~~~

[Note topdir]

GHC needs various support files (library packages, RTS etc), plus
various auxiliary programs (cp, gcc, etc).  It starts by finding topdir,
the root of GHC's support files

On Unix:
  - ghc always has a shell wrapper that passes a -B<dir> option

On Windows:
  - ghc never has a shell wrapper.
  - we can find the location of the ghc binary, which is
        $topdir/<foo>/<something>.exe
    where <something> may be "ghc", "ghc-stage2", or similar
  - we strip off the "<foo>/<something>.exe" to leave $topdir.

from topdir we can find package.conf, ghc-asm, etc.


SysTools.initSysProgs figures out exactly where all the auxiliary programs
are, and initialises mutable variables to make it easy to call them.
To to this, it makes use of definitions in Config.hs, which is a Haskell
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

initLlvmTargets :: Maybe String
                -> IO LlvmTargets
initLlvmTargets mbMinusB
  = do top_dir <- findTopDir mbMinusB
       let llvmTargetsFile = top_dir </> "llvm-targets"
       llvmTargetsStr <- readFile llvmTargetsFile
       case maybeReadFuzzy llvmTargetsStr of
         Just s -> return (fmap mkLlvmTarget <$> s)
         Nothing -> pgmError ("Can't parse " ++ show llvmTargetsFile)
  where
    mkLlvmTarget :: (String, String, String) -> LlvmTarget
    mkLlvmTarget (dl, cpu, attrs) = LlvmTarget dl cpu (words attrs)


initSysTools :: Maybe String    -- Maybe TopDir path (without the '-B' prefix)
             -> IO Settings     -- Set all the mutable variables above, holding
                                --      (a) the system programs
                                --      (b) the package-config file
                                --      (c) the GHC usage message
initSysTools mbMinusB
  = do top_dir <- findTopDir mbMinusB
             -- see [Note topdir]
             -- NB: top_dir is assumed to be in standard Unix
             -- format, '/' separated

       let settingsFile = top_dir </> "settings"
           platformConstantsFile = top_dir </> "platformConstants"
           installed :: FilePath -> FilePath
           installed file = top_dir </> file
           libexec :: FilePath -> FilePath
           libexec file = top_dir </> "bin" </> file

       settingsStr <- readFile settingsFile
       platformConstantsStr <- readFile platformConstantsFile
       mySettings <- case maybeReadFuzzy settingsStr of
                     Just s ->
                         return s
                     Nothing ->
                         pgmError ("Can't parse " ++ show settingsFile)
       platformConstants <- case maybeReadFuzzy platformConstantsStr of
                            Just s ->
                                return s
                            Nothing ->
                                pgmError ("Can't parse " ++
                                          show platformConstantsFile)
       let getSetting key = case lookup key mySettings of
                            Just xs ->
                                return $ case stripPrefix "$topdir" xs of
                                         Just [] ->
                                             top_dir
                                         Just xs'@(c:_)
                                          | isPathSeparator c ->
                                             top_dir ++ xs'
                                         _ ->
                                             xs
                            Nothing -> pgmError ("No entry for " ++ show key ++ " in " ++ show settingsFile)
           getBooleanSetting key = case lookup key mySettings of
                                   Just "YES" -> return True
                                   Just "NO" -> return False
                                   Just xs -> pgmError ("Bad value for " ++ show key ++ ": " ++ show xs)
                                   Nothing -> pgmError ("No entry for " ++ show key ++ " in " ++ show settingsFile)
           readSetting key = case lookup key mySettings of
                             Just xs ->
                                 case maybeRead xs of
                                 Just v -> return v
                                 Nothing -> pgmError ("Failed to read " ++ show key ++ " value " ++ show xs)
                             Nothing -> pgmError ("No entry for " ++ show key ++ " in " ++ show settingsFile)
       crossCompiling <- getBooleanSetting "cross compiling"
       targetArch <- readSetting "target arch"
       targetOS <- readSetting "target os"
       targetWordSize <- readSetting "target word size"
       targetUnregisterised <- getBooleanSetting "Unregisterised"
       targetHasGnuNonexecStack <- readSetting "target has GNU nonexec stack"
       targetHasIdentDirective <- readSetting "target has .ident directive"
       targetHasSubsectionsViaSymbols <- readSetting "target has subsections via symbols"
       myExtraGccViaCFlags <- getSetting "GCC extra via C opts"
       -- On Windows, mingw is distributed with GHC,
       -- so we look in TopDir/../mingw/bin
       -- It would perhaps be nice to be able to override this
       -- with the settings file, but it would be a little fiddly
       -- to make that possible, so for now you can't.
       gcc_prog <- getSetting "C compiler command"
       gcc_args_str <- getSetting "C compiler flags"
       gccSupportsNoPie <- getBooleanSetting "C compiler supports -no-pie"
       cpp_prog <- getSetting "Haskell CPP command"
       cpp_args_str <- getSetting "Haskell CPP flags"
       let unreg_gcc_args = if targetUnregisterised
                            then ["-DNO_REGS", "-DUSE_MINIINTERPRETER"]
                            else []
           -- TABLES_NEXT_TO_CODE affects the info table layout.
           tntc_gcc_args
            | mkTablesNextToCode targetUnregisterised
               = ["-DTABLES_NEXT_TO_CODE"]
            | otherwise = []
           cpp_args= map Option (words cpp_args_str)
           gcc_args = map Option (words gcc_args_str
                               ++ unreg_gcc_args
                               ++ tntc_gcc_args)
       ldSupportsCompactUnwind <- getBooleanSetting "ld supports compact unwind"
       ldSupportsBuildId       <- getBooleanSetting "ld supports build-id"
       ldSupportsFilelist      <- getBooleanSetting "ld supports filelist"
       ldIsGnuLd               <- getBooleanSetting "ld is GNU ld"
       perl_path <- getSetting "perl command"

       let pkgconfig_path = installed "package.conf.d"
           ghc_usage_msg_path  = installed "ghc-usage.txt"
           ghci_usage_msg_path = installed "ghci-usage.txt"

             -- For all systems, unlit, split, mangle are GHC utilities
             -- architecture-specific stuff is done when building Config.hs
           unlit_path = libexec cGHC_UNLIT_PGM

             -- split is a Perl script
           split_script  = libexec cGHC_SPLIT_PGM

       windres_path <- getSetting "windres command"
       libtool_path <- getSetting "libtool command"
       ar_path <- getSetting "ar command"
       ranlib_path <- getSetting "ranlib command"

       tmpdir <- getTemporaryDirectory

       touch_path <- getSetting "touch command"

       let -- On Win32 we don't want to rely on #!/bin/perl, so we prepend
           -- a call to Perl to get the invocation of split.
           -- On Unix, scripts are invoked using the '#!' method.  Binary
           -- installations of GHC on Unix place the correct line on the
           -- front of the script at installation time, so we don't want
           -- to wire-in our knowledge of $(PERL) on the host system here.
           (split_prog,  split_args)
             | isWindowsHost = (perl_path,    [Option split_script])
             | otherwise     = (split_script, [])
       mkdll_prog <- getSetting "dllwrap command"
       let mkdll_args = []

       -- cpp is derived from gcc on all platforms
       -- HACK, see setPgmP below. We keep 'words' here to remember to fix
       -- Config.hs one day.


       -- Other things being equal, as and ld are simply gcc
       gcc_link_args_str <- getSetting "C compiler link flags"
       let   as_prog  = gcc_prog
             as_args  = gcc_args
             ld_prog  = gcc_prog
             ld_args  = gcc_args ++ map Option (words gcc_link_args_str)

       -- We just assume on command line
       lc_prog <- getSetting "LLVM llc command"
       lo_prog <- getSetting "LLVM opt command"
       lcc_prog <- getSetting "LLVM clang command"

       let iserv_prog = libexec "ghc-iserv"

       let platform = Platform {
                          platformArch = targetArch,
                          platformOS   = targetOS,
                          platformWordSize = targetWordSize,
                          platformUnregisterised = targetUnregisterised,
                          platformHasGnuNonexecStack = targetHasGnuNonexecStack,
                          platformHasIdentDirective = targetHasIdentDirective,
                          platformHasSubsectionsViaSymbols = targetHasSubsectionsViaSymbols,
                          platformIsCrossCompiling = crossCompiling
                      }

       return $ Settings {
                    sTargetPlatform = platform,
                    sTmpDir         = normalise tmpdir,
                    sGhcUsagePath   = ghc_usage_msg_path,
                    sGhciUsagePath  = ghci_usage_msg_path,
                    sTopDir         = top_dir,
                    sRawSettings    = mySettings,
                    sExtraGccViaCFlags = words myExtraGccViaCFlags,
                    sSystemPackageConfig = pkgconfig_path,
                    sLdSupportsCompactUnwind = ldSupportsCompactUnwind,
                    sLdSupportsBuildId       = ldSupportsBuildId,
                    sLdSupportsFilelist      = ldSupportsFilelist,
                    sLdIsGnuLd               = ldIsGnuLd,
                    sGccSupportsNoPie        = gccSupportsNoPie,
                    sProgramName             = "ghc",
                    sProjectVersion          = cProjectVersion,
                    sPgm_L   = unlit_path,
                    sPgm_P   = (cpp_prog, cpp_args),
                    sPgm_F   = "",
                    sPgm_c   = (gcc_prog, gcc_args),
                    sPgm_s   = (split_prog,split_args),
                    sPgm_a   = (as_prog, as_args),
                    sPgm_l   = (ld_prog, ld_args),
                    sPgm_dll = (mkdll_prog,mkdll_args),
                    sPgm_T   = touch_path,
                    sPgm_windres = windres_path,
                    sPgm_libtool = libtool_path,
                    sPgm_ar = ar_path,
                    sPgm_ranlib = ranlib_path,
                    sPgm_lo  = (lo_prog,[]),
                    sPgm_lc  = (lc_prog,[]),
                    sPgm_lcc = (lcc_prog,[]),
                    sPgm_i   = iserv_prog,
                    sOpt_L       = [],
                    sOpt_P       = [],
                    sOpt_F       = [],
                    sOpt_c       = [],
                    sOpt_a       = [],
                    sOpt_l       = [],
                    sOpt_windres = [],
                    sOpt_lcc     = [],
                    sOpt_lo      = [],
                    sOpt_lc      = [],
                    sOpt_i       = [],
                    sPlatformConstants = platformConstants
             }

-- returns a Unix-format path (relying on getBaseDir to do so too)
findTopDir :: Maybe String -- Maybe TopDir path (without the '-B' prefix).
           -> IO String    -- TopDir (in Unix format '/' separated)
findTopDir (Just minusb) = return (normalise minusb)
findTopDir Nothing
    = do -- Get directory of executable
         maybe_exec_dir <- getBaseDir
         case maybe_exec_dir of
             -- "Just" on Windows, "Nothing" on unix
             Nothing  -> throwGhcExceptionIO (InstallationError "missing -B<dir> option")
             Just dir -> return dir

{- Note [Windows stack usage]

See: Trac #8870 (and #8834 for related info) and #12186

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

-----------------------------------------------------------------------------
-- Define       getBaseDir     :: IO (Maybe String)

getBaseDir :: IO (Maybe String)
#if defined(mingw32_HOST_OS)
-- Assuming we are running ghc, accessed by path  $(stuff)/<foo>/ghc.exe,
-- return the path $(stuff)/lib.
getBaseDir = try_size 2048 -- plenty, PATH_MAX is 512 under Win32.
  where
    try_size size = allocaArray (fromIntegral size) $ \buf -> do
        ret <- c_GetModuleFileName nullPtr buf size
        case ret of
          0 -> return Nothing
          _ | ret < size -> do
                path <- peekCWString buf
                real <- getFinalPath path -- try to resolve symlinks paths
                let libdir = (rootDir . sanitize . maybe path id) real
                exists <- doesDirectoryExist libdir
                if exists
                   then return $ Just libdir
                   else fail path
            | otherwise  -> try_size (size * 2)

    -- getFinalPath returns paths in full raw form.
    -- Unfortunately GHC isn't set up to handle these
    -- So if the call succeeded, we need to drop the
    -- \\?\ prefix.
    sanitize s = if "\\\\?\\" `isPrefixOf` s
                    then drop 4 s
                    else s

    rootDir s = case splitFileName $ normalise s of
                (d, ghc_exe)
                 | lower ghc_exe `elem` ["ghc.exe",
                                         "ghc-stage1.exe",
                                         "ghc-stage2.exe",
                                         "ghc-stage3.exe"] ->
                    case splitFileName $ takeDirectory d of
                    -- ghc is in $topdir/bin/ghc.exe
                    (d', _) -> takeDirectory d' </> "lib"
                _ -> fail s

    fail s = panic ("can't decompose ghc.exe path: " ++ show s)
    lower = map toLower

foreign import WINDOWS_CCONV unsafe "windows.h GetModuleFileNameW"
  c_GetModuleFileName :: Ptr () -> CWString -> Word32 -> IO Word32

-- Attempt to resolve symlinks in order to find the actual location GHC
-- is located at. See Trac #11759.
getFinalPath :: FilePath -> IO (Maybe FilePath)
getFinalPath name = do
    dllHwnd <- failIfNull "LoadLibrary"     $ loadLibrary "kernel32.dll"
    -- Note: The API GetFinalPathNameByHandleW is only available starting from Windows Vista.
    -- This means that we can't bind directly to it since it may be missing.
    -- Instead try to find it's address at runtime and if we don't succeed consider the
    -- function failed.
    addr_m  <- (fmap Just $ failIfNull "getProcAddress" $ getProcAddress dllHwnd "GetFinalPathNameByHandleW")
                  `catch` (\(_ :: SomeException) -> return Nothing)
    case addr_m of
      Nothing   -> return Nothing
      Just addr -> do handle  <- failIf (==iNVALID_HANDLE_VALUE) "CreateFile"
                                        $ createFile name
                                                     gENERIC_READ
                                                     fILE_SHARE_READ
                                                     Nothing
                                                     oPEN_EXISTING
                                                     (fILE_ATTRIBUTE_NORMAL .|. fILE_FLAG_BACKUP_SEMANTICS)
                                                     Nothing
                      let fnPtr = makeGetFinalPathNameByHandle $ castPtrToFunPtr addr
                      -- First try to resolve the path to get the actual path
                      -- of any symlinks or other file system redirections that
                      -- may be in place. However this function can fail, and in
                      -- the event it does fail, we need to try using the
                      -- original path and see if we can decompose that.
                      -- If the call fails Win32.try will raise an exception
                      -- that needs to be caught. See #14159
                      path    <- (Win32.try "GetFinalPathName"
                                    (\buf len -> fnPtr handle buf len 0) 512
                                    `finally` closeHandle handle)
                                `catch`
                                 (\(_ :: IOException) -> return name)
                      return $ Just path

type GetFinalPath = HANDLE -> LPTSTR -> DWORD -> DWORD -> IO DWORD

foreign import WINDOWS_CCONV unsafe "dynamic"
  makeGetFinalPathNameByHandle :: FunPtr GetFinalPath -> GetFinalPath
#else
getBaseDir = return Nothing
#endif

linkDynLib :: DynFlags -> [String] -> [InstalledUnitId] -> IO ()
linkDynLib dflags0 o_files dep_packages
 = do
    let -- This is a rather ugly hack to fix dynamically linked
        -- GHC on Windows. If GHC is linked with -threaded, then
        -- it links against libHSrts_thr. But if base is linked
        -- against libHSrts, then both end up getting loaded,
        -- and things go wrong. We therefore link the libraries
        -- with the same RTS flags that we link GHC with.
        dflags1 = if cGhcThreaded then addWay' WayThreaded dflags0
                                  else                     dflags0
        dflags2 = if cGhcDebugged then addWay' WayDebug dflags1
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
           WayDyn `elem` ways dflags
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
                          filter ((/= rtsUnitId) . packageConfigId) pkgs
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
              )
        _ -> do
            -------------------------------------------------------------------
            -- Making a DSO
            -------------------------------------------------------------------

            let output_fn = case o_file of { Just s -> s; Nothing -> "a.out"; }
            let bsymbolicFlag = -- we need symbolic linking to resolve
                                -- non-PIC intra-package-relocations
                                ["-Wl,-Bsymbolic"]

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

getPkgFrameworkOpts :: DynFlags -> Platform -> [InstalledUnitId] -> IO [String]
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
