{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module GHC (
    array, base, binary, bytestring, cabal, checkApiAnnotations, compiler,
    containers, compareSizes, deepseq, deriveConstants, directory, dllSplit,
    filepath, genapply, genprimopcode, ghc, ghcBoot, ghcBootTh, ghcCabal, ghci,
    ghcPkg, ghcPrim, ghcTags, ghcSplit, haddock, haskeline, hsc2hs, hoopl, hp2ps,
    hpc, hpcBin, integerGmp, integerSimple, iservBin, libffi, mkUserGuidePart,
    parallel, pretty, primitive, process, rts, runGhc, stm, templateHaskell,
    terminfo, time, touchy, transformers, unlit, unix, win32, xhtml,

    defaultKnownPackages, stageDirectory, programPath
    ) where

import Base
import Context
import Package
import Stage

-- | These are all GHC packages we know about. Build rules will be generated for
-- all of them. However, not all of these packages will be built. For example,
-- package 'win32' is built only on Windows.
-- "Packages" defines default conditions for building each package, which can
-- be overridden in @hadrian/src/UserSettings.hs@.
defaultKnownPackages :: [Package]
defaultKnownPackages =
    [ array, base, binary, bytestring, cabal, checkApiAnnotations, compiler
    , containers, compareSizes, deepseq, deriveConstants, directory, dllSplit
    , filepath, genapply, genprimopcode, ghc, ghcBoot, ghcBootTh, ghcCabal, ghci
    , ghcPkg, ghcPrim, ghcTags, haddock, haskeline, hsc2hs, hoopl, hp2ps, hpc
    , hpcBin, integerGmp, integerSimple, iservBin, libffi, mkUserGuidePart
    , parallel, pretty, primitive, process, rts, runGhc, stm, templateHaskell
    , terminfo, time, touchy, transformers, unlit, unix, win32, xhtml ]

-- | Package definitions, see 'Package'.
array               = library  "array"
base                = library  "base"
binary              = library  "binary"
bytestring          = library  "bytestring"
cabal               = library  "Cabal"        `setPath` "libraries/Cabal/Cabal"
checkApiAnnotations = utility  "check-api-annotations"
compiler            = topLevel "ghc"          `setPath` "compiler"
containers          = library  "containers"
compareSizes        = utility  "compareSizes" `setPath` "utils/compare_sizes"
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
ghci                = library  "ghci"
ghcPkg              = utility  "ghc-pkg"
ghcPrim             = library  "ghc-prim"
ghcTags             = utility  "ghctags"
haddock             = utility  "haddock"
haskeline           = library  "haskeline"
hsc2hs              = utility  "hsc2hs"
hoopl               = library  "hoopl"
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

-- | @ghc-split@ is a Perl script used by GHC with @-split-objs@ flag. It is
-- generated in "Rules.Generators.GhcSplit".
ghcSplit :: FilePath
ghcSplit = "inplace/lib/bin/ghc-split"

-- | Relative path to the directory containing build artefacts of a given 'Stage'.
stageDirectory :: Stage -> FilePath
stageDirectory = stageString

-- TODO: Create a separate rule for copying executables to inplace/bin
-- TODO: move to buildRootPath, see #113
-- | The 'FilePath' to a program executable in a given 'Context'.
programPath :: Context -> Maybe FilePath
programPath Context {..} = lookup (stage, package) exes
  where
    exes = [ inplace2 checkApiAnnotations
           , install1 compareSizes
           , inplace0 deriveConstants
           , inplace0 dllSplit
           , inplace0 genapply
           , inplace0 genprimopcode
           , inplace0 ghc             `setFile` "ghc-stage1"
           , inplace1 ghc             `setFile` "ghc-stage2"
           , install0 ghcCabal
           , inplace1 ghcCabal
           , inplace0 ghcPkg
           , install1 ghcPkg
           , inplace2 ghcTags
           , inplace2 haddock
           , inplace0 hp2ps
           , inplace1 hpcBin          `setFile` "hpc"
           , inplace0 hsc2hs
           , install1 hsc2hs
           , install1 iservBin
           , inplace0 mkUserGuidePart
           , inplace1 runGhc          `setFile` "runhaskell"
           , inplace0 touchy          `setDir`  "inplace/lib/bin"
           , inplace0 unlit           `setDir`  "inplace/lib/bin" ]
    inplace  pkg = programInplacePath -/- pkgNameString pkg <.> exe
    inplace0 pkg = ((Stage0, pkg), inplace pkg)
    inplace1 pkg = ((Stage1, pkg), inplace pkg)
    inplace2 pkg = ((Stage2, pkg), inplace pkg)
    install stage pkg = pkgPath package -/- stageDirectory stage -/- "build"
                                        -/- pkgNameString pkg <.> exe
    install0 pkg = ((Stage0, pkg), install Stage0 pkg)
    install1 pkg = ((Stage1, pkg), install Stage1 pkg)
    setFile ((stage, pkg), x) y = ((stage, pkg), takeDirectory x -/- y <.> exe)
    setDir  ((stage, pkg), x) y = ((stage, pkg), y -/- takeFileName x)
