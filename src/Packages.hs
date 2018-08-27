{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Packages (
    -- * GHC packages
    array, base, binary, bytestring, cabal, checkApiAnnotations, checkPpr,
    compareSizes, compiler, containers, deepseq, deriveConstants, directory,
    filepath, genapply, genprimopcode, ghc, ghcBoot, ghcBootTh, ghcCompact,
    ghcHeap, ghci, ghcPkg, ghcPrim, ghcTags, ghcSplit, haddock, haskeline,
    hsc2hs, hp2ps, hpc, hpcBin, integerGmp, integerSimple, iserv, libffi,
    libiserv, mtl, parsec, parallel, pretty, primitive, process, rts, runGhc,
    stm, templateHaskell, terminfo, text, time, timeout, touchy, transformers,
    unlit, unix, win32, xhtml, ghcPackages, isGhcPackage,

    -- * Package information
    programName, nonHsMainPackage, autogenPath, programPath, timeoutPath,
    buildDll0, rtsContext, rtsBuildPath, libffiContext, libffiBuildPath,
    libffiLibraryName
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
    [ array, base, binary, bytestring, cabal, checkPpr, checkApiAnnotations
    , compareSizes, compiler, containers, deepseq, deriveConstants, directory
    , filepath, genapply, genprimopcode, ghc, ghcBoot, ghcBootTh, ghcCompact
    , ghcHeap, ghci, ghcPkg, ghcPrim, ghcTags, haddock, haskeline, hsc2hs, hp2ps
    , hpc, hpcBin, integerGmp, integerSimple, iserv, libffi, libiserv, mtl
    , parsec, parallel, pretty, process, rts, runGhc, stm, templateHaskell
    , terminfo, text, time, touchy, transformers, unlit, unix, win32, xhtml
    , timeout ]

-- TODO: Optimise by switching to sets of packages.
isGhcPackage :: Package -> Bool
isGhcPackage = (`elem` ghcPackages)

-- | Package definitions, see 'Package'.
array               = hsLib  "array"
base                = hsLib  "base"
binary              = hsLib  "binary"
bytestring          = hsLib  "bytestring"
cabal               = hsLib  "Cabal"           `setPath` "libraries/Cabal/Cabal"
checkApiAnnotations = hsUtil "check-api-annotations"
checkPpr            = hsUtil "check-ppr"
compareSizes        = hsUtil "compareSizes"    `setPath` "utils/compare_sizes"
compiler            = hsTop  "ghc"             `setPath` "compiler"
containers          = hsLib  "containers"
deepseq             = hsLib  "deepseq"
deriveConstants     = hsUtil "deriveConstants"
directory           = hsLib  "directory"
filepath            = hsLib  "filepath"
genapply            = hsUtil "genapply"
genprimopcode       = hsUtil "genprimopcode"
ghc                 = hsPrg  "ghc-bin"         `setPath` "ghc"
ghcBoot             = hsLib  "ghc-boot"
ghcBootTh           = hsLib  "ghc-boot-th"
ghcCompact          = hsLib  "ghc-compact"
ghcHeap             = hsLib  "ghc-heap"
ghci                = hsLib  "ghci"
ghcPkg              = hsUtil "ghc-pkg"
ghcPrim             = hsLib  "ghc-prim"
ghcTags             = hsUtil "ghctags"
ghcSplit            = hsUtil "ghc-split"
haddock             = hsUtil "haddock"
haskeline           = hsLib  "haskeline"
hsc2hs              = hsUtil "hsc2hs"
hp2ps               = hsUtil "hp2ps"
hpc                 = hsLib  "hpc"
hpcBin              = hsUtil "hpc-bin"         `setPath` "utils/hpc"
integerGmp          = hsLib  "integer-gmp"
integerSimple       = hsLib  "integer-simple"
iserv               = hsUtil "iserv"
libffi              = cTop   "libffi"
libiserv            = hsLib  "libiserv"
mtl                 = hsLib  "mtl"
parsec              = hsLib  "parsec"
parallel            = hsLib  "parallel"
pretty              = hsLib  "pretty"
primitive           = hsLib  "primitive"
process             = hsLib  "process"
rts                 = cTop   "rts"
runGhc              = hsUtil "runghc"
stm                 = hsLib  "stm"
templateHaskell     = hsLib  "template-haskell"
terminfo            = hsLib  "terminfo"
text                = hsLib  "text"
time                = hsLib  "time"
timeout             = hsUtil "timeout"         `setPath` "testsuite/timeout"
touchy              = hsUtil "touchy"
transformers        = hsLib  "transformers"
unlit               = hsUtil "unlit"
unix                = hsLib  "unix"
win32               = hsLib  "Win32"
xhtml               = hsLib  "xhtml"

-- | Construct a Haskell library package, e.g. @array@.
hsLib :: PackageName -> Package
hsLib name = hsLibrary name ("libraries" -/- name)

-- | Construct a top-level Haskell library package, e.g. @compiler@.
hsTop :: PackageName -> Package
hsTop name = hsLibrary name name

-- | Construct a top-level C library package, e.g. @rts@.
cTop :: PackageName -> Package
cTop name = cLibrary name name

-- | Construct a top-level Haskell program package, e.g. @ghc@.
hsPrg :: PackageName -> Package
hsPrg name = hsProgram name name

-- | Construct a Haskell utility package, e.g. @haddock@.
hsUtil :: PackageName -> Package
hsUtil name = hsProgram name ("utils" -/- name)

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
    -- Also, why @runhaskell@ instead of @runghc@?
    return $ prefix ++ case package of
                              p | p == ghc    -> "ghc"
                                | p == hpcBin -> "hpc"
                                | p == runGhc -> "runhaskell"
                                | p == iserv  -> "ghc-iserv"
                              _               -> pkgName package

-- | The 'FilePath' to a program executable in a given 'Context'.
programPath :: Context -> Action FilePath
programPath context@Context {..} = do
    -- TODO: The @touchy@ utility lives in the @lib/bin@ directory instead of
    -- @bin@, which is likely just a historical accident that should be fixed.
    -- See: https://github.com/snowleopard/hadrian/issues/570
    -- Likewise for 'unlit'.
    name <- programName context
    path <- if package `elem` [touchy, unlit] then stageLibPath stage <&> (-/- "bin")
                                              else stageBinPath stage
    return $ path -/- name <.> exe

-- TODO: Move @timeout@ to the @util@ directory and build in a more standard
-- location like other programs used only by the testsuite.
timeoutPath :: FilePath
timeoutPath = "testsuite/timeout/install-inplace/bin/timeout" <.> exe

-- TODO: Can we extract this information from Cabal files?
-- | Some program packages should not be linked with Haskell main function.
nonHsMainPackage :: Package -> Bool
nonHsMainPackage = (`elem` [ghc, hp2ps, iserv, touchy, unlit])

-- TODO: Can we extract this information from Cabal files?
-- | Path to the @autogen@ directory generated when configuring a package.
autogenPath :: Context -> Action FilePath
autogenPath context@Context {..}
    | isLibrary package = autogen "build"
    | package == ghc    = autogen "build/ghc"
    | package == hpcBin = autogen "build/hpc"
    | otherwise         = autogen $ "build" -/- pkgName package
  where
    autogen dir = contextPath context <&> (-/- dir -/- "autogen")

buildDll0 :: Context -> Action Bool
buildDll0 Context {..} = do
    windows <- windowsHost
    return $ windows && stage == Stage1 && package == compiler

-- | RTS is considered a Stage1 package.
rtsContext :: Context
rtsContext = vanillaContext Stage1 rts

-- | Path to the RTS build directory.
rtsBuildPath :: Action FilePath
rtsBuildPath = buildPath rtsContext

-- | The 'libffi' library is considered a 'Stage1' package.
libffiContext :: Context
libffiContext = vanillaContext Stage1 libffi

-- | Build directory for in-tree 'libffi' library.
libffiBuildPath :: Action FilePath
libffiBuildPath = buildPath libffiContext

-- | Name of the 'libffi' library.
libffiLibraryName :: Action FilePath
libffiLibraryName = do
    useSystemFfi <- flag UseSystemFfi
    windows      <- windowsHost
    return $ case (useSystemFfi, windows) of
        (True , False) -> "ffi"
        (False, False) -> "Cffi"
        (_    , True ) -> "Cffi-6"
