module Settings.Packages (
    module Settings.Default,
    packages, getPackages, knownPackages, findKnownPackage
    ) where

import Package
import Expression
import Predicates
import Oracles.Setting
import Settings.User
import Settings.Default

-- Combining default list of packages with user modifications
packages :: Packages
packages = defaultPackages <> userPackages

getPackages :: Expr [Package]
getPackages = fromDiffExpr packages

-- These are the packages we build by default
defaultPackages :: Packages
defaultPackages = mconcat
    [ stage0 ? packagesStage0
    , stage1 ? packagesStage1 ]

packagesStage0 :: Packages
packagesStage0 = mconcat
    [ append [binPackageDb, binary, cabal, compiler, hoopl, hpc, transformers]
    , notWindowsHost ? notTargetOs "ios" ? append [terminfo] ]

-- TODO: what do we do with parallel, stm, random, primitive, vector and dph?
packagesStage1 :: Packages
packagesStage1 = mconcat
    [ packagesStage0
    , append [ array, base, bytestring, containers, deepseq, directory
             , filepath, ghcPrim, haskeline, integerLibrary, pretty, process
             , templateHaskell, time ]
    , windowsHost    ? append [win32]
    , notWindowsHost ? append [unix]
    , buildHaddock   ? append [xhtml] ]

knownPackages :: [Package]
knownPackages = defaultKnownPackages ++ userKnownPackages

-- Note: this is slow but we keep it simple as there not too many packages (30)
findKnownPackage :: PackageName -> Maybe Package
findKnownPackage name = find (\pkg -> pkgName pkg == name) knownPackages
