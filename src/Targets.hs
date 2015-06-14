module Targets (
    targetWays, targetPackages, targetDirectory,
    allPackages,
    customConfigureSettings,
    array, base, binPackageDb, binary, bytestring, cabal, compiler, containers,
    deepseq, directory, filepath, ghcPrim, haskeline, hoopl, hpc,
    integerLibrary, parallel, pretty, primitive, process, stm, templateHaskell,
    terminfo, time, transformers, unix, win32, xhtml
    ) where

import Ways hiding (parallel)
import Base hiding (arg, args, Args, TargetDir)
import Package
import Switches
import Expression
import Expression.Settings

-- These are the packages we build
targetPackages :: Packages
targetPackages = mconcat
    [ stage Stage0 ? packagesStage0
    , stage Stage1 ? packagesStage1 ]

packagesStage0 :: Packages
packagesStage0 = mconcat
    [ append [binPackageDb, binary, cabal, compiler, hoopl, hpc, transformers]
    , notWindowsHost ? notTargetOs "ios" ? append [terminfo] ]

packagesStage1 :: Packages
packagesStage1 = mconcat
    [ append [ array, base, bytestring, containers, deepseq, directory
             , filepath, ghcPrim, haskeline, integerLibrary, parallel
             , pretty, primitive, process, stm, templateHaskell, time ]
    , windowsHost    ? append [win32]
    , notWindowsHost ? append [unix]
    , buildHaddock   ? append [xhtml] ]

-- Packages will be build these ways
targetWays :: Ways
targetWays = mconcat
    [                              append [vanilla] -- always build vanilla
    , notStage Stage0            ? append [profiling]
    , platformSupportsSharedLibs ? append [dynamic] ]

-- Build results will be placed into a target directory with the following
-- typical structure:
-- * build/           : contains compiled object code
-- * doc/             : produced by haddock
-- * package-data.mk  : contains output of ghc-cabal applied to pkgCabal
targetDirectory :: Stage -> Package -> FilePath
targetDirectory stage package
    | package == compiler = "stage" ++ show (fromEnum stage + 1)
    | stage   == Stage0   = "dist-boot"
    | otherwise           = "dist-install"

-- Package definitions
allPackages :: [Package]
allPackages =
    [ array, base, binPackageDb, binary, bytestring, cabal, compiler
    , containers, deepseq, directory, filepath, ghcPrim, haskeline
    , hoopl, hpc, integerLibrary, parallel, pretty, primitive, process
    , stm, templateHaskell, terminfo, time, transformers, unix, win32, xhtml ]

array           = library  "array"
base            = library  "base"
binPackageDb    = library  "bin-package-db"
binary          = library  "binary"
bytestring      = library  "bytestring"
cabal           = library  "Cabal/Cabal" `setCabal` "Cabal.cabal"
compiler        = topLevel "compiler"    `setCabal` "ghc.cabal"
containers      = library  "containers"
deepseq         = library  "deepseq"
directory       = library  "directory"
filepath        = library  "filepath"
ghcPrim         = library  "ghc-prim"
haskeline       = library  "haskeline"
hoopl           = library  "hoopl"
hpc             = library  "hpc"
integerLibrary  = library  integerLibraryName `setCabal` integerLibraryCabal
parallel        = library  "parallel"
pretty          = library  "pretty"
primitive       = library  "primitive"
process         = library  "process"
stm             = library  "stm"
templateHaskell = library  "template-haskell"
terminfo        = library  "terminfo"
time            = library  "time"
transformers    = library  "transformers"
unix            = library  "unix"
win32           = library  "Win32"
xhtml           = library  "xhtml"

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

-- Custom configure settings for packages
customConfigureSettings :: Settings
customConfigureSettings = mconcat
    [ package integerLibrary ?
      windowsHost     ? appendSub "--configure-option" ["--with-intree-gmp"]
    , package base    ? appendSub "--flags" [integerLibraryName]
    , package ghcPrim ? appendSub "--flag"  ["include-ghc-prim"]]

-- Note [Cabal name weirdness]
-- Find out if we can move the contents to just Cabal/
-- What is Cabal/cabal-install? Do we need it?
-- A related question about gmp2 -- let's rename the cabal file?
