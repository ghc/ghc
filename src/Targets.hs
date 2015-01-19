module Targets (targetPackages, targetPackagesInStage) where

import Package.Base

-- These are the packages we build:
-- TODO: this should eventually be removed and replaced by the top-level
-- target, i.e. GHC (and perhaps, something else)
targetPackages :: [Package]
targetPackages = [ standardLibrary "array"            [Stage1]
                 , standardLibrary "bin-package-db"   [Stage0, Stage1]
                 , standardLibrary "binary"           [Stage0, Stage1]
                 , standardLibrary "bytestring"       [Stage1]
                 -- see Note [Cabal package weirdness]
                 , standardLibrary "Cabal/Cabal"      [Stage0, Stage1]
                 , standardLibrary "containers"       [Stage1]
                 , standardLibrary "deepseq"          [Stage1]
                 , standardLibrary "directory"        [Stage1]
                 , standardLibrary "filepath"         [Stage1]
                 , standardLibrary "hoopl"            [Stage0, Stage1]
                 , standardLibrary "hpc"              [Stage0, Stage1]
                 , standardLibrary "ghc-prim"         [Stage1]
                 , standardLibrary "parallel"         [Stage1]
                 , standardLibrary "pretty"           [Stage1]
                 , standardLibrary "stm"              [Stage1]
                 , standardLibrary "template-haskell" [Stage1]
                 , standardLibrary "transformers"     [Stage0, Stage1]
                 ]

targetPackagesInStage :: Stage -> [Package]
targetPackagesInStage stage = filter inStage targetPackages
  where
    inStage (Package _ _ todoItems) = any matchStage todoItems
    matchStage (todoStage, _, _)    = todoStage == stage

-- Note [Cabal package weirdness]
-- Find out if we can move the contents to just Cabal/
-- What is Cabal/cabal-install? Do we need it?
