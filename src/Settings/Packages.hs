module Settings.Packages (getPackages, knownPackages, findKnownPackage) where

import Expression
import Predicates
import Settings.User

-- Combining default list of packages with user modifications
getPackages :: Expr [Package]
getPackages = fromDiffExpr $ defaultPackages <> userPackages

-- These are the packages we build by default
-- TODO: simplify
defaultPackages :: Packages
defaultPackages = mconcat
    [ stage0 ? packagesStage0
    , stage1 ? packagesStage1
    , stage2 ? packagesStage2 ]

packagesStage0 :: Packages
packagesStage0 = mconcat
    [ append [ ghcBoot, binary, cabal, compiler, ghc, ghcCabal, ghcPkg
             , hsc2hs, hoopl, hpc, templateHaskell, transformers ]
    , stage0 ? append [deriveConstants, genapply, genprimopcode, hp2ps]
    , notM windowsHost ? notM (anyHostOs ["ios"]) ? append [terminfo] ]

-- TODO: what do we do with parallel, stm, random, primitive, vector and dph?
packagesStage1 :: Packages
packagesStage1 = mconcat
    [ packagesStage0
    , append [ array, base, bytestring, containers, compareSizes, deepseq
             , directory, dllSplit, filepath
             , ghcPrim, ghcPwd, haskeline, hpcBin, integerLibrary
             , mkUserGuidePart, pretty, process, runghc, time ]
    , windowsHost      ? append [win32]
    , notM windowsHost ? append [unix]
    , buildHaddock     ? append [xhtml] ]

packagesStage2 :: Packages
packagesStage2 = mconcat
    [ append [ghcTags]
    , buildHaddock ? append [haddock] ]

knownPackages :: [Package]
knownPackages = defaultKnownPackages ++ userKnownPackages

-- Note: this is slow but we keep it simple as there not too many packages (30)
findKnownPackage :: PackageName -> Maybe Package
findKnownPackage name = find (\pkg -> pkgName pkg == name) knownPackages
