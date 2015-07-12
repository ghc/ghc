module Targets (
    defaultTargetDirectory,
    array, base, binPackageDb, binary, bytestring, cabal, compiler, containers,
    deepseq, directory, filepath, ghcPrim, haskeline, hoopl, hpc,
    integerGmp, integerGmp2, integerSimple,
    parallel, pretty, primitive, process, stm, templateHaskell,
    terminfo, time, transformers, unix, win32, xhtml
    ) where

import Base hiding (arg, args)
import Package

-- Build results will be placed into a target directory with the following
-- typical structure:
-- * build/           : contains compiled object code
-- * doc/             : produced by haddock
-- * package-data.mk  : contains output of ghc-cabal applied to pkgCabal
defaultTargetDirectory :: Stage -> Package -> FilePath
defaultTargetDirectory stage package
    | package == compiler = "stage" ++ show (fromEnum stage + 1)
    | stage   == Stage0   = "dist-boot"
    | otherwise           = "dist-install"

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
integerGmp      = library  "integer-gmp"
integerGmp2     = library  "integer-gmp2" `setCabal` "integer-gmp.cabal"
integerSimple   = library  "integer-simple"
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

-- Note [Cabal name weirdness]
-- Find out if we can move the contents to just Cabal/
-- What is Cabal/cabal-install? Do we need it?
-- A related question about gmp2 -- let's rename the cabal file?
