module Targets (
    targetDirectory,
    knownPackages,
    customConfigureSettings,
    array, base, binPackageDb, binary, bytestring, cabal, compiler, containers,
    deepseq, directory, filepath, ghcPrim, haskeline, hoopl, hpc,
    integerLibrary, parallel, pretty, primitive, process, stm, templateHaskell,
    terminfo, time, transformers, unix, win32, xhtml
    ) where

import Base hiding (arg, args)
import Package
import Switches
import Expression

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

-- These are all packages we know about. Build rules will be generated for
-- all of them. However, not all of these packages will be built. For example,
-- package 'win32' is built only on Windows.
-- Settings/Packages.hs defines default conditions for building each package,
-- which can be overridden in UserSettings.hs.
knownPackages :: [Package]
knownPackages =
    [ array, base, binPackageDb, binary, bytestring, cabal, compiler
    , containers, deepseq, directory, filepath, ghcPrim, haskeline
    , hoopl, hpc, integerLibrary, parallel, pretty, primitive, process
    , stm, templateHaskell, terminfo, time, transformers, unix, win32, xhtml ]

-- Package definitions
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
