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
    [ append [ binary, cabal, compiler, ghc, ghcBoot, ghcCabal, ghcPkg
             , hsc2hs, hoopl, hpc, templateHaskell, transformers ]
    , stage0 ? append [deriveConstants, dllSplit, genapply, genprimopcode, hp2ps]
    , notM windowsHost ? notM (anyHostOs ["ios"]) ? append [terminfo] ]

-- TODO: what do we do with parallel, stm, random, primitive, vector and dph?
packagesStage1 :: Packages
packagesStage1 = mconcat
    [ packagesStage0
    , append [ array, base, bytestring, containers, compareSizes, deepseq
             , directory, filepath, ghci, ghcPrim, ghcPwd, haskeline, hpcBin
             , integerLibrary, mkUserGuidePart, pretty, process, runghc, time ]
    , windowsHost      ? append [win32]
    , notM windowsHost ? append [unix]
    , notM windowsHost ? append [iservBin]
    , buildHaddock     ? append [xhtml] ]

packagesStage2 :: Packages
packagesStage2 = mconcat
    [ append [ghcTags]
    , buildHaddock ? append [haddock] ]

-- TODO: switch to Set Package as the order of packages should not matter?
knownPackages :: [Package]
knownPackages = sort $ defaultKnownPackages ++ userKnownPackages

-- Note: this is slow but we keep it simple as there are just ~50 packages
findKnownPackage :: PackageName -> Maybe Package
findKnownPackage name = find (\pkg -> pkgName pkg == name) knownPackages
