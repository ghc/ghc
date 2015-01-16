module Targets (libraryPackageNames) where

import Base

-- These are the packages we build:
-- TODO: this should eventually be removed and replaced by the top-level
-- target, i.e. GHC (and perhaps, something else)
libraryPackageNames :: Stage -> [String]
libraryPackageNames Stage0 =
    [ "bin-package-db"
    , "binary"
    , "hoopl"
    , "hpc"
    , "transformers" ]
libraryPackageNames Stage1 = libraryPackageNames Stage0 ++
    [ "array"
    , "deepseq"
    , "Cabal/Cabal"
    , "containers"
    , "filepath"
    , "parallel"
    , "pretty"
    , "stm"
    , "template-haskell" ]
libraryPackageNames _ = error "Not implemented"
