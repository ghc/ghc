module Settings (
    getArgs, getLibraryWays, getRtsWays, flavour, knownPackages,
    findPackageByName, unsafeFindPackageByName, unsafeFindPackageByPath,
    isLibrary, stagePackages, programContext, getIntegerPackage
    ) where

import CommandLine
import Expression
import Flavour
import Packages
import UserSettings (userFlavours, userPackages, userDefaultFlavour)

import {-# SOURCE #-} Settings.Default
import Settings.Flavours.Benchmark
import Settings.Flavours.Development
import Settings.Flavours.Llvm
import Settings.Flavours.Performance
import Settings.Flavours.Profiled
import Settings.Flavours.Quick
import Settings.Flavours.Quickest
import Settings.Flavours.QuickCross
import Settings.Flavours.GhcInGhci

getArgs :: Args
getArgs = expr flavour >>= args

getLibraryWays :: Ways
getLibraryWays = expr flavour >>= libraryWays

getRtsWays :: Ways
getRtsWays = expr flavour >>= rtsWays

stagePackages :: Stage -> Action [Package]
stagePackages stage = do
    f <- flavour
    packages f stage

hadrianFlavours :: [Flavour]
hadrianFlavours =
    [ benchmarkFlavour, defaultFlavour, developmentFlavour Stage1
    , developmentFlavour Stage2, performanceFlavour, profiledFlavour
    , quickFlavour, quickestFlavour, quickCrossFlavour, benchmarkLlvmFlavour
    , performanceLlvmFlavour, profiledLlvmFlavour, quickLlvmFlavour
    , ghcInGhciFlavour ]

flavour :: Action Flavour
flavour = do
    flavourName <- fromMaybe userDefaultFlavour <$> cmdFlavour
    let unknownFlavour = error $ "Unknown build flavour: " ++ flavourName
        flavours       = hadrianFlavours ++ userFlavours
    return $ fromMaybe unknownFlavour $ find ((== flavourName) . name) flavours

getIntegerPackage :: Expr Package
getIntegerPackage = expr (integerLibrary =<< flavour)

-- TODO: there is duplication and inconsistency between this and
-- Rules.Program.getProgramContexts. There should only be one way to get a
-- context / contexts for a given stage and package.
programContext :: Stage -> Package -> Action Context
programContext stage pkg = do
    profiled <- ghcProfiled <$> flavour
    dynGhcProgs <- dynamicGhcPrograms =<< flavour
    return $ Context stage pkg (wayFor profiled dynGhcProgs)

    where wayFor prof dyn
            | prof && dyn                          =
                error "programContext: profiling+dynamic not supported"
            | pkg == ghc && prof && stage > Stage0 = profiling
            | dyn && stage > Stage0                = dynamic
            | otherwise                            = vanilla

-- TODO: switch to Set Package as the order of packages should not matter?
-- Otherwise we have to keep remembering to sort packages from time to time.
knownPackages :: [Package]
knownPackages = sort $ ghcPackages ++ userPackages

-- TODO: Speed up? Switch to Set?
-- Note: this is slow but we keep it simple as there are just ~50 packages
findPackageByName :: PackageName -> Maybe Package
findPackageByName name = find (\pkg -> pkgName pkg == name) knownPackages

unsafeFindPackageByName :: PackageName -> Package
unsafeFindPackageByName name = fromMaybe (error msg) $ findPackageByName name
  where
    msg = "unsafeFindPackageByName: No package with name " ++ name

unsafeFindPackageByPath :: FilePath -> Package
unsafeFindPackageByPath path = err $ find (\pkg -> resPkgPath pkg == path) knownPackages
  where
    err = fromMaybe $ error ("findPackageByPath: No package for path " ++ path)
