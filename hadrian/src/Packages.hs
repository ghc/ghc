{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Packages (
    -- * GHC packages
    array, base, binary, bytestring, cabal, checkPpr,
    checkExact, countDeps,
    compareSizes, compiler, containers, deepseq, deriveConstants, directory,
    exceptions, filepath, genapply, genprimopcode, ghc, ghcAbiHash, ghcBignum, ghcBoot,
    ghcBootTh, ghcCompact, ghcHeap, ghci, ghciWrapper, ghcPkg, ghcPrim, haddock, haskeline,
    hsc2hs, hp2ps, hpc, hpcBin, integerGmp, integerSimple, iserv, iservProxy,
    libffi, libiserv, mtl, parsec, pretty, primitive, process, remoteIserv, rts,
    runGhc, stm, templateHaskell, terminfo, text, time, timeout, touchy,
    transformers, unlit, unix, win32, xhtml, ghcPackages, isGhcPackage,

    -- * Package information
    programName, nonHsMainPackage, autogenPath, globalAutogenPath, programPath, timeoutPath,
    rtsContext, rtsBuildPath, libffiBuildPath,
    ensureConfigured
    ) where

import Hadrian.Package
import Hadrian.Utilities

import Base
import Context
import Oracles.Flag
import Oracles.Setting

-- | These are all GHC packages we know about. Build rules will be generated for
-- all of them. However, not all of these packages will be built. For example,
-- package 'win32' is built only on Windows. @GHC.defaultPackages@ defines
-- default conditions for building each package. Users can add their own
-- packages and modify build default build conditions in "UserSettings".
ghcPackages :: [Package]
ghcPackages =
    [ array, base, binary, bytestring, cabal, checkPpr, checkExact, countDeps
    , compareSizes, compiler, containers, deepseq, deriveConstants, directory
    , exceptions, filepath, genapply, genprimopcode, ghc, ghcAbiHash, ghcBignum, ghcBoot
    , ghcBootTh , ghcCompact, ghcHeap, ghci, ghciWrapper, ghcPkg, ghcPrim, haddock, haskeline
    , hsc2hs , hp2ps, hpc, hpcBin, integerGmp, integerSimple, iserv, libffi, libiserv, mtl
    , parsec, pretty, process, rts, runGhc, stm, templateHaskell
    , terminfo, text, time, touchy, transformers, unlit, unix, win32, xhtml
    , timeout ]

-- TODO: Optimise by switching to sets of packages.
isGhcPackage :: Package -> Bool
isGhcPackage = (`elem` ghcPackages)

-- | Package definitions, see 'Package'.
array               = lib  "array"
base                = lib  "base"
binary              = lib  "binary"
bytestring          = lib  "bytestring"
cabal               = lib  "Cabal"           `setPath` "libraries/Cabal/Cabal"
checkPpr            = util "check-ppr"
checkExact          = util "check-exact"
countDeps           = util "count-deps"
compareSizes        = util "compareSizes"    `setPath` "utils/compare_sizes"
compiler            = top  "ghc"             `setPath` "compiler"
containers          = lib  "containers"      `setPath` "libraries/containers/containers"
deepseq             = lib  "deepseq"
deriveConstants     = util "deriveConstants"
directory           = lib  "directory"
exceptions          = lib  "exceptions"
filepath            = lib  "filepath"
genapply            = util "genapply"
genprimopcode       = util "genprimopcode"
ghc                 = prg  "ghc-bin"         `setPath` "ghc"
ghcAbiHash          = lib  "ghc-abihash"
ghcBignum           = lib  "ghc-bignum"
ghcBoot             = lib  "ghc-boot"
ghcBootTh           = lib  "ghc-boot-th"
ghcCompact          = lib  "ghc-compact"
ghcHeap             = lib  "ghc-heap"
ghci                = lib  "ghci"
ghciWrapper         = prg  "ghci-wrapper"    `setPath` "driver/ghci"
                      -- See Note [Hadrian's ghci-wrapper package]
ghcPkg              = util "ghc-pkg"
ghcPrim             = lib  "ghc-prim"
haddock             = util "haddock"
haskeline           = lib  "haskeline"
hsc2hs              = util "hsc2hs"
hp2ps               = util "hp2ps"
hpc                 = lib  "hpc"
hpcBin              = util "hpc-bin"         `setPath` "utils/hpc"
integerGmp          = lib  "integer-gmp"
integerSimple       = lib  "integer-simple"
iserv               = util "iserv"
iservProxy          = util "iserv-proxy"
libffi              = top  "libffi"
libiserv            = lib  "libiserv"
mtl                 = lib  "mtl"
parsec              = lib  "parsec"
pretty              = lib  "pretty"
primitive           = lib  "primitive"
process             = lib  "process"
remoteIserv         = util "remote-iserv"
rts                 = top  "rts"
runGhc              = util "runghc"
stm                 = lib  "stm"
templateHaskell     = lib  "template-haskell"
terminfo            = lib  "terminfo"
text                = lib  "text"
time                = lib  "time"
timeout             = util "timeout"         `setPath` "testsuite/timeout"
touchy              = util "touchy"
transformers        = lib  "transformers"
unlit               = util "unlit"
unix                = lib  "unix"
win32               = lib  "Win32"
xhtml               = lib  "xhtml"

-- | Construct a library package, e.g. @array@.
lib :: PackageName -> Package
lib name = library name ("libraries" -/- name)

-- | Construct a top-level library package, e.g. @compiler@.
top :: PackageName -> Package
top name = library name name

-- | Construct a top-level program package, e.g. @ghc@.
prg :: PackageName -> Package
prg name = program name name

-- | Construct a utility package, e.g. @haddock@.
util :: PackageName -> Package
util name = program name ("utils" -/- name)

-- | Amend a package path if it doesn't conform to a typical pattern.
setPath :: Package -> FilePath -> Package
setPath pkg path = pkg { pkgPath = path }

-- | Given a 'Context', compute the name of the program that is built in it
-- assuming that the corresponding package's type is 'Program'. For example, GHC
-- built in 'Stage0' is called @ghc-stage1@. If the given package is a
-- 'Library', the function simply returns its name.
programName :: Context -> Action String
programName Context {..} = do
    cross <- flag CrossCompiling
    targetPlatform <- setting TargetPlatformFull
    let prefix = if cross then targetPlatform ++ "-" else ""
    -- TODO: Can we extract this information from Cabal files?
    -- Alp: We could, but then the iserv package would have to
    --      use Cabal conditionals + a 'profiling' flag
    --      to declare the executable name, and I'm not sure
    --      this is allowed (or desired for that matter).
    return $ prefix ++ basename
  where
    basename
      | package == ghc          = "ghc"
      | package == ghciWrapper  = "ghci" -- See Note [Hadrian's ghci-wrapper package]
      | package == hpcBin       = "hpc"
      | package == iserv        = "ghc-iserv" ++ concat [
                                        if wayUnit' `wayUnit` way
                                            then suffix
                                            else ""
                                        | (wayUnit', suffix) <- [
                                            (Profiling, "-prof"),
                                            (Dynamic,   "-dyn")
                                        ]]
      | otherwise               = pkgName package

-- | The 'FilePath' to a program executable in a given 'Context'.
programPath :: Context -> Action FilePath
programPath context@Context {..} = do
    -- TODO: The @touchy@ utility lives in the @lib/bin@ directory instead of
    -- @bin@, which is likely just a historical accident that should be fixed.
    -- See: https://github.com/snowleopard/hadrian/issues/570
    -- Likewise for @iserv@ and @unlit@.
    name <- programName context
    path <- if package `elem` [iserv, touchy, unlit]
              then stageLibPath stage <&> (-/- "bin")
              else stageBinPath stage
    return $ path -/- name <.> exe

-- TODO: Move @timeout@ to the @util@ directory and build in a more standard
-- location like other programs used only by the testsuite.
timeoutPath :: FilePath
timeoutPath = "testsuite/timeout/install-inplace/bin/timeout" <.> exe

-- TODO: Can we extract this information from Cabal files?
-- | Some program packages should not be linked with Haskell main function.
nonHsMainPackage :: Package -> Bool
nonHsMainPackage = (`elem` [hp2ps, iserv, touchy, unlit, ghciWrapper])

-- TODO: Combine this with 'programName'.
-- | Path to the @autogen@ directory generated by 'buildAutogenFiles'.
autogenPath :: Context -> Action FilePath
autogenPath context@Context {..}
    | isLibrary package = autogen "build"
    | package == ghc    = autogen "build/ghc"
    | package == hpcBin = autogen "build/hpc"
    | package == ghciWrapper = autogen "build/ghci"
                               -- See Note [Hadrian's ghci-wrapper package]
    | otherwise         = autogen $ "build" -/- pkgName package
  where
    autogen dir = contextPath context <&> (-/- dir -/- "autogen")

globalAutogenPath :: Context -> Action FilePath
globalAutogenPath context= autogen "build"
  where
    autogen dir = contextPath context <&> (-/- dir -/- "global-autogen")

-- | Make sure a given context has already been fully configured. The
-- implementation simply calls 'need' on the context's @autogen/cabal_macros.h@
-- file, which triggers 'configurePackage' and 'buildAutogenFiles'. Why this
-- indirection? Going via @autogen/cabal_macros.h@ allows us to cache the
-- configuration steps, i.e. not to repeat them if they have already been done.
ensureConfigured :: Context -> Action ()
ensureConfigured context = do
    autogen <- autogenPath context
    need [autogen -/- "cabal_macros.h"]

-- | RTS is considered a Stage1 package. This determines RTS build directory.
rtsContext :: Stage -> Context
rtsContext stage = vanillaContext stage rts

-- | Path to the RTS build directory.
rtsBuildPath :: Stage -> Action FilePath
rtsBuildPath stage = buildPath (rtsContext stage)

-- | Build directory for in-tree 'libffi' library.
libffiBuildPath :: Stage -> Action FilePath
libffiBuildPath stage = buildPath $ Context
    stage
    libffi
    (error "libffiBuildPath: way not set.")

{-
Note [Hadrian's ghci-wrapper package]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
On Linux platforms the `ghci` executable is a shell-script wrapper produced
by the binary distribution `install` rule. However, this approach is not
viable work on Windows platforms, where binary distributions are usable
directly after unzipping, without any need for the user to run `make install`.

Moreover, Windows has rather special requirements regarding console setup and
teardown. Consequently on Windows ghci.exe is a purpose-built executable, the
C source of which is found in driver/ghci. Getting Hadrian to build this via
Cabal requires a few headstands:

 - Hadrian generally assumes that the name of the executable produced by a
   'Program' package is the same as the package name. However, this is not
   the case here: we name the package `ghci-wrapper` to avoid conflicting
   with the `ghci` library yet we want the final executable to be named
   `ghci.exe`. We accomplish this by overriding 'Packages.programName'.

 - The executable requires a few C sources (which live in `driver/utils`) in
   addition to the main ghci.c. Ideally these would be built independently as
   a static library which could then be linked into the executable;
   unfortunately Cabal doesn't support this. We instead add the sources to
   the C-Sources list in the Cabal file.

 - Unfortunately, Cabal/Hadrian's handling of C-sources appears to fall on
   its face when a relative path is used (e.g. `../cwrapper.c`). Consequently
   we copy the files into `driver/ghci` in the configure script.
-}
