module Settings.Packages (getPackages, knownPackages, findKnownPackage) where

import Base
import Expression
import GHC
import Predicates
import Oracles.Config.Setting
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
    -- the stage0 predicate makes sure these packages are built only in Stage0
    , stage0 ? append [deriveConstants, dllSplit, genapply, genprimopcode, hp2ps]
    , stage0 ? windowsHost ? append [touchy]
    , notM windowsHost ? notM iosHost ? append [terminfo] ]

packagesStage1 :: Packages
packagesStage1 = mconcat
    [ packagesStage0
    , append [ array, base, bytestring, containers, compareSizes, deepseq
             , directory, filepath, ghci, ghcPrim, haskeline, hpcBin
             , integerLibrary, mkUserGuidePart, pretty, process, rts, runGhc
             , time ]
    , windowsHost      ? append [win32]
    , notM windowsHost ? append [unix]
    , notM windowsHost ? append [iservBin]
    , buildHaddock     ? append [xhtml] ]

-- TODO: currently there is an unchecked assumption that we build only programs
-- in Stage2 and Stage3. Can we check this in compile time?
packagesStage2 :: Packages
packagesStage2 = mconcat
    [ append [ghcTags]
    , buildHaddock ? append [haddock] ]

-- TODO: switch to Set Package as the order of packages should not matter?
-- Otherwise we have to keep remembering to sort packages from time to time.
knownPackages :: [Package]
knownPackages = sort $ defaultKnownPackages ++ userKnownPackages

-- Note: this is slow but we keep it simple as there are just ~50 packages
-- TODO: speed up?
findKnownPackage :: PackageName -> Maybe Package
findKnownPackage name = find (\pkg -> pkgName pkg == name) knownPackages
