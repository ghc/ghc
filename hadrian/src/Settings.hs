module Settings (
    getArgs, getLibraryWays, getRtsWays, flavour, knownPackages,
    findPackageByName, isLibrary, stagePackages, programContext,
    getIntegerPackage
    ) where

import CommandLine
import Expression
import Flavour
import Packages
import UserSettings (userFlavours, userPackages, userDefaultFlavour)

import {-# SOURCE #-} Settings.Default
import Settings.Flavours.Development
import Settings.Flavours.Performance
import Settings.Flavours.Profiled
import Settings.Flavours.Quick
import Settings.Flavours.Quickest
import Settings.Flavours.QuickCross

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
    [ defaultFlavour, developmentFlavour Stage1, developmentFlavour Stage2
    , performanceFlavour, profiledFlavour, quickFlavour, quickestFlavour
    , quickCrossFlavour ]

flavour :: Action Flavour
flavour = do
    flavourName <- fromMaybe userDefaultFlavour <$> cmdFlavour
    let unknownFlavour = error $ "Unknown build flavour: " ++ flavourName
        flavours       = hadrianFlavours ++ userFlavours
    return $ fromMaybe unknownFlavour $ find ((== flavourName) . name) flavours

getIntegerPackage :: Expr Package
getIntegerPackage = expr (integerLibrary =<< flavour)

programContext :: Stage -> Package -> Action Context
programContext stage pkg = do
    profiled <- ghcProfiled <$> flavour
    return $ if pkg == ghc && profiled && stage > Stage0
             then Context stage pkg profiling
             else vanillaContext stage pkg

-- TODO: switch to Set Package as the order of packages should not matter?
-- Otherwise we have to keep remembering to sort packages from time to time.
knownPackages :: [Package]
knownPackages = sort $ ghcPackages ++ userPackages

-- TODO: Speed up? Switch to Set?
-- Note: this is slow but we keep it simple as there are just ~50 packages
findPackageByName :: PackageName -> Maybe Package
findPackageByName name = find (\pkg -> pkgName pkg == name) knownPackages
