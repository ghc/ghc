module Targets (
    targetWays, targetPackages, targetDirectories,
    customConfigureSettings,
    array, base, binPackageDb, binary, bytestring, cabal, containers, deepseq,
    directory, filepath, ghcPrim, haskeline, hoopl, hpc, integerLibrary,
    parallel, pretty, primitive, process, stm, templateHaskell, terminfo, time,
    transformers, unix, win32, xhtml
    ) where

import qualified Ways
import Base hiding (arg, args, Args, TargetDir)
import Package
import Switches
import Expression
import Expression.Settings

-- These are the packages we build
targetPackages :: Packages Action
targetPackages = mconcat
    [ stage Stage0 ? packagesStage0
    , stage Stage1 ? packagesStage1 ]

packagesStage0 :: Packages Action
packagesStage0 = mconcat
    [ return [ binPackageDb, binary, cabal, hoopl, hpc, transformers ]
    , notWindowsHost ? notTargetOs "ios" ? return [terminfo] ]

packagesStage1 :: Packages Action
packagesStage1 = mconcat
    [ packagesStage0
    , return [ array, base, bytestring, containers, deepseq, directory
             , filepath, ghcPrim, haskeline, integerLibrary, parallel
             , pretty, primitive, process, stm, templateHaskell, time ]
    , notWindowsHost ? return [unix]
    , windowsHost    ? return [win32]
    , buildHaddock   ? return [xhtml] ]

-- Packages will be build these ways
targetWays :: Ways Action
targetWays = mconcat
    [                              return [Ways.vanilla] -- always build vanilla
    , notStage Stage0            ? return [Ways.profiling]
    , platformSupportsSharedLibs ? return [Ways.dynamic] ]

-- Build results will be placed into a target directory with the following
-- typical structure:
-- * build/           : contains compiled object code
-- * doc/             : produced by haddock
-- * package-data.mk  : contains output of ghc-cabal applied to pkgCabal
targetDirectories :: Monad m => TargetDir m
targetDirectories = do
    stage   <- asks getStage
    package <- asks getPackage
    let targetDir
            | package == compiler = "stage" ++ show (succ stage)
            | stage   == Stage0   = "dist-boot"
            | otherwise           = "dist-install"
    return targetDir

-- Package definitions
array, base, binPackageDb, binary, bytestring, cabal, containers, deepseq,
    directory, filepath, ghcPrim, haskeline, hoopl, hpc, integerLibrary,
    parallel, pretty, primitive, process, stm, templateHaskell, terminfo, time,
    transformers, unix, win32, xhtml :: Package

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
customConfigureSettings :: Settings Action
customConfigureSettings = mconcat
    [ package base           ? arg ("--flags=" ++ integerLibraryName)
    , package ghcPrim        ? arg "--flag=include-ghc-prim"
    , package integerLibrary ?
        windowsHost ? arg "--configure-option=--with-intree-gmp"
    ]

-- Note [Cabal name weirdness]
-- Find out if we can move the contents to just Cabal/
-- What is Cabal/cabal-install? Do we need it?
-- A related question about gmp2 -- let's rename the cabal file?
