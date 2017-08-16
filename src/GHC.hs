{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module GHC (
    -- * GHC packages
    array, base, binary, bytestring, cabal, checkApiAnnotations, compareSizes,
    compiler, containers, deepseq, deriveConstants, directory, dllSplit, filepath,
    genapply, genprimopcode, ghc, ghcBoot, ghcBootTh, ghcCabal, ghcCompact, ghci,
    ghcPkg, ghcPrim, ghcTags, ghcSplit, haddock, haskeline, hsc2hs, hp2ps,
    hpc, hpcBin, integerGmp, integerSimple, iservBin, libffi, mkUserGuidePart,
    parallel, pretty, primitive, process, rts, runGhc, stm, templateHaskell,
    terminfo, time, touchy, transformers, unlit, unix, win32, xhtml,
    defaultKnownPackages,

    -- * Package information
    builderProvenance, programName, nonCabalContext, nonHsMainPackage, autogenPath,

    -- * RTS library
    rtsContext, rtsBuildPath, rtsConfIn,

    -- * Miscellaneous
    ghcSplitPath, stripCmdPath, inplaceInstallPath, buildDll0
    ) where

import Base
import Context
import Oracles.Setting

-- | These are all GHC packages we know about. Build rules will be generated for
-- all of them. However, not all of these packages will be built. For example,
-- package 'win32' is built only on Windows.
-- "Packages" defines default conditions for building each package, which can
-- be overridden in @hadrian/src/UserSettings.hs@.
defaultKnownPackages :: [Package]
defaultKnownPackages =
    [ array, base, binary, bytestring, cabal, checkApiAnnotations, compareSizes
    , compiler, containers, deepseq, deriveConstants, directory, dllSplit
    , filepath, genapply, genprimopcode, ghc, ghcBoot, ghcBootTh, ghcCabal
    , ghcCompact, ghci, ghcPkg, ghcPrim, ghcTags, haddock, haskeline, hsc2hs
    , hp2ps, hpc, hpcBin, integerGmp, integerSimple, iservBin, libffi
    , mkUserGuidePart, parallel, pretty, primitive, process, rts, runGhc, stm
    , templateHaskell, terminfo, time, touchy, transformers, unlit, unix, win32
    , xhtml ]

-- | Package definitions, see 'Package'.
array               = library  "array"
base                = library  "base"
binary              = library  "binary"
bytestring          = library  "bytestring"
cabal               = library  "Cabal"        `setPath` "libraries/Cabal/Cabal"
checkApiAnnotations = utility  "check-api-annotations"
compareSizes        = utility  "compareSizes" `setPath` "utils/compare_sizes"
compiler            = topLevel "ghc"          `setPath` "compiler"
containers          = library  "containers"
deepseq             = library  "deepseq"
deriveConstants     = utility  "deriveConstants"
directory           = library  "directory"
dllSplit            = utility  "dll-split"
filepath            = library  "filepath"
genapply            = utility  "genapply"
genprimopcode       = utility  "genprimopcode"
ghc                 = topLevel "ghc-bin"      `setPath` "ghc"   `setType` Program
ghcBoot             = library  "ghc-boot"
ghcBootTh           = library  "ghc-boot-th"
ghcCabal            = utility  "ghc-cabal"
ghcCompact          = library  "ghc-compact"
ghci                = library  "ghci"
ghcPkg              = utility  "ghc-pkg"
ghcPrim             = library  "ghc-prim"
ghcTags             = utility  "ghctags"
ghcSplit            = utility  "ghc-split"
haddock             = utility  "haddock"
haskeline           = library  "haskeline"
hsc2hs              = utility  "hsc2hs"
hp2ps               = utility  "hp2ps"
hpc                 = library  "hpc"
hpcBin              = utility  "hpc-bin"      `setPath` "utils/hpc"
integerGmp          = library  "integer-gmp"
integerSimple       = library  "integer-simple"
iservBin            = topLevel "iserv-bin"    `setPath` "iserv" `setType` Program
libffi              = topLevel "libffi"
mkUserGuidePart     = utility  "mkUserGuidePart"
parallel            = library  "parallel"
pretty              = library  "pretty"
primitive           = library  "primitive"
process             = library  "process"
rts                 = topLevel "rts"
runGhc              = utility  "runghc"
stm                 = library  "stm"
templateHaskell     = library  "template-haskell"
terminfo            = library  "terminfo"
time                = library  "time"
touchy              = utility  "touchy"
transformers        = library  "transformers"
unlit               = utility  "unlit"
unix                = library  "unix"
win32               = library  "Win32"
xhtml               = library  "xhtml"

-- | Some builders are built by this very build system, in which case
-- 'builderProvenance' returns the corresponding build 'Context' (which includes
-- 'Stage' and GHC 'Package').
builderProvenance :: Builder -> Maybe Context
builderProvenance = \case
    DeriveConstants  -> context Stage0 deriveConstants
    GenApply         -> context Stage0 genapply
    GenPrimopCode    -> context Stage0 genprimopcode
    Ghc _ Stage0     -> Nothing
    Ghc _ stage      -> context (pred stage) ghc
    GhcCabal         -> context Stage0 ghcCabal
    GhcCabalHsColour -> builderProvenance $ GhcCabal
    GhcPkg _ Stage0  -> Nothing
    GhcPkg _ _       -> context Stage0 ghcPkg
    Haddock          -> context Stage2 haddock
    Hpc              -> context Stage1 hpcBin
    Hsc2Hs           -> context Stage0 hsc2hs
    Unlit            -> context Stage0 unlit
    _                -> Nothing
  where
    context s p = Just $ vanillaContext s p

-- | Given a 'Context', compute the name of the program that is built in it
-- assuming that the corresponding package's type is 'Program'. For example, GHC
-- built in 'Stage0' is called @ghc-stage1@. If the given package is a
-- 'Library', the function simply returns its name.
programName :: Context -> String
programName Context {..}
    | package == ghc      = "ghc-stage" ++ show (fromEnum stage + 1)
    | package == hpcBin   = "hpc"
    | package == runGhc   = "runhaskell"
    | package == iservBin = "ghc-iserv"
    | otherwise           = pkgNameString package

-- | Some contexts are special: their packages do not have @.cabal@ metadata or
-- we cannot run @ghc-cabal@ on them, e.g. because the latter hasn't been built
-- yet (this is the case with the 'ghcCabal' package in 'Stage0').
nonCabalContext :: Context -> Bool
nonCabalContext Context {..} = (package `elem` [hp2ps, rts, touchy, unlit])
    || package == ghcCabal && stage == Stage0

-- | Some program packages should not be linked with Haskell main function.
nonHsMainPackage :: Package -> Bool
nonHsMainPackage = (`elem` [ghc, hp2ps, iservBin, touchy, unlit])

-- | Path to the autogen directory generated by @ghc-cabal@ of a given 'Context'.
autogenPath :: Context -> Action FilePath
autogenPath context@Context {..}
    | isLibrary package   = autogen "build"
    | package == ghc      = autogen "build/ghc"
    | package == hpcBin   = autogen "build/hpc"
    | package == iservBin = autogen "build/iserv"
    | otherwise           = autogen $ "build" -/- pkgNameString package
  where
    autogen dir = buildPath context <&> (-/- dir -/- "autogen")

-- | Given a 'Package', return the path where the corresponding program is
-- installed. Most programs are installed in 'programInplacePath'.
inplaceInstallPath :: Package -> FilePath
inplaceInstallPath pkg
    | pkg == touchy   = inplaceLibBinPath
    | pkg == unlit    = inplaceLibBinPath
    | pkg == iservBin = inplaceLibBinPath
    | otherwise       = inplaceBinPath

-- | @ghc-split@ is a Perl script used by GHC with @-split-objs@ flag. It is
-- generated in "Rules.Generators.GhcSplit".
ghcSplitPath :: FilePath
ghcSplitPath = inplaceLibBinPath -/- "ghc-split"

-- ref: mk/config.mk
-- | Command line tool for stripping.
stripCmdPath :: Action FilePath
stripCmdPath = do
    targetPlatform <- setting TargetPlatform
    top <- topDirectory
    case targetPlatform of
        "x86_64-unknown-mingw32" ->
             return (top -/- "inplace/mingw/bin/strip.exe")
        "arm-unknown-linux" ->
             return ":" -- HACK: from the make-based system, see the ref above
        _ -> return "strip"

-- TODO: Move to RTS-specific package?
-- | RTS is considered a Stage1 package. This determines RTS build directory.
rtsContext :: Context
rtsContext = vanillaContext Stage1 rts

-- | Path to the RTS build directory.
rtsBuildPath :: Action FilePath
rtsBuildPath = buildPath rtsContext

-- | Path to RTS package configuration file, to be processed by HsCpp.
rtsConfIn :: FilePath
rtsConfIn = pkgPath rts -/- "package.conf.in"

buildDll0 :: Context -> Action Bool
buildDll0 Context {..} = do
    windows <- windowsHost
    return $ windows && stage == Stage1 && package == compiler
