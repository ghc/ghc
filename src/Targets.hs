module Targets (libraryPackages, libraryPackagesInStage) where

import Base

-- These are the packages we build:
-- TODO: this should eventually be removed and replaced by the top-level
-- target, i.e. GHC (and perhaps, something else)
libraryPackagesInStage :: Stage -> [String]
libraryPackagesInStage Stage0 =
    [ "bin-package-db"
    , "binary"
    , "hoopl"
    , "hpc"
    , "transformers" ]
libraryPackagesInStage Stage1 =
    libraryPackagesInStage Stage0 ++
    [ "array"
    , "deepseq"
    , "Cabal/Cabal"
    , "containers"
    , "filepath"
    , "parallel"
    , "pretty"
    , "stm"
    , "template-haskell" ]

libraryPackagesInStage _ = []

libraryPackages :: [String]
libraryPackages = concatMap libraryPackagesInStage [Stage0 ..]
