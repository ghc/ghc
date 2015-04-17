{-# LANGUAGE NoImplicitPrelude #-}
module Targets (
    buildHaddock,
    targetWays, targetPackages,
    IntegerLibraryImpl (..), integerLibraryImpl, integerLibraryName,
    array, base, binPackageDb, binary, bytestring, cabal, containers, deepseq,
    directory, filepath, ghcPrim, haskeline, hoopl, hpc, integerLibrary,
    parallel, pretty, primitive, process, stm, templateHaskell, terminfo, time,
    transformers, unix, win32, xhtml
    ) where

import qualified Ways
import Base
import Package
import Expression.Base

buildHaddock :: BuildPredicate
buildHaddock = true

-- These are the packages we build
targetPackages :: Packages
targetPackages = msum
    [ stage Stage0 ? packagesStage0
    , stage Stage1 ? packagesStage1 ]

packagesStage0 :: Packages
packagesStage0 = msum
    [ fromList [ binPackageDb, binary, cabal, hoopl, hpc, transformers ]
    , not windowsHost && not (targetOs "ios") ? return terminfo ]

packagesStage1 :: Packages
packagesStage1 = msum
    [ packagesStage0
    , fromList [ array, base, bytestring, containers, deepseq, directory
               , filepath, ghcPrim, haskeline, integerLibrary, parallel
               , pretty, primitive, process, stm, templateHaskell, time ]
    , not windowsHost ? return unix
    , windowsHost     ? return win32
    , buildHaddock    ? return xhtml ]

-- Packages will be build these ways
targetWays :: Ways
targetWays = msum
    [                              return Ways.vanilla -- always build vanilla
    , notStage Stage0            ? return Ways.profiling
    , platformSupportsSharedLibs ? return Ways.dynamic ]

-- Build results will be placed into a target directory with the following
-- typical structure:
-- * build/           : contains compiled object code
-- * doc/             : produced by haddock
-- * package-data.mk  : contains output of ghc-cabal applied to pkgCabal
targetDirectories :: FilePaths
targetDirectories =
    stage Stage0 ?? (return "dist-boot", return "dist-install")

-- Support for multiple integer library implementations
data IntegerLibraryImpl = IntegerGmp | IntegerGmp2 | IntegerSimple

integerLibraryImpl :: IntegerLibraryImpl
integerLibraryImpl = IntegerGmp2

integerLibraryName :: String
integerLibraryName = case integerLibraryImpl of
    IntegerGmp    -> "integer-gmp"
    IntegerGmp2   -> "integer-gmp2"
    IntegerSimple -> "integer-simple"

-- see Note [Cabal name weirdness]
integerLibraryCabal :: FilePath
integerLibraryCabal = case integerLibraryImpl of
        IntegerGmp    -> "integer-gmp.cabal"
        IntegerGmp2   -> "integer-gmp.cabal" -- Indeed, why make life easier?
        IntegerSimple -> "integer-simple.cabal"

-- Package definitions
array, base, binPackageDb, binary, bytestring, cabal, containers, deepseq,
    directory, filepath, ghcPrim, haskeline, hoopl, hpc, integerLibrary,
    parallel, pretty, primitive, process, stm, templateHaskell, terminfo, time,
    transformers, unix, win32, xhtml :: Package

array           = library "array"
base            = library "base"
binPackageDb    = library "bin-package-db"
binary          = library "binary"
bytestring      = library "bytestring"
cabal           = library "Cabal/Cabal" `setCabal` "Cabal.cabal"
containers      = library "containers"
deepseq         = library "deepseq"
directory       = library "directory"
filepath        = library "filepath"
ghcPrim         = library "ghc-prim"
haskeline       = library "haskeline"
hoopl           = library "hoopl"
hpc             = library "hpc"
integerLibrary  = library integerLibraryName `setCabal` integerLibraryCabal
parallel        = library "parallel"
pretty          = library "pretty"
primitive       = library "primitive"
process         = library "process"
stm             = library "stm"
templateHaskell = library "template-haskell"
terminfo        = library "terminfo"
time            = library "time"
transformers    = library "transformers"
unix            = library "unix"
win32           = library "Win32"
xhtml           = library "xhtml"

-- TODISCUSS
-- Note [Cabal name weirdness]
-- Find out if we can move the contents to just Cabal/
-- What is Cabal/cabal-install? Do we need it?
-- A related question about gmp2 -- let's rename the cabal file?
