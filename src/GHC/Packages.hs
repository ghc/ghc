{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module GHC.Packages where

import Hadrian.Package
import Hadrian.Utilities

-- | These are all GHC packages we know about. Build rules will be generated for
-- all of them. However, not all of these packages will be built. For example,
-- package 'win32' is built only on Windows. 'defaultPackages' defines default
-- conditions for building each package. Users can add their own packages and
-- modify build default build conditions in "UserSettings".
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
ghcCabal            = hsUtil "ghc-cabal"
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
touchy              = hsUtil "touchy"
transformers        = hsLib  "transformers"
unlit               = hsUtil "unlit"
unix                = hsLib  "unix"
win32               = hsLib  "Win32"
xhtml               = hsLib  "xhtml"
timeout             = hsUtil "timeout"         `setPath` "testsuite/timeout"

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
