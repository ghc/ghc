module Settings (
    getArgs, getPackages, getLibraryWays, getRtsWays, flavour, knownPackages,
    findKnownPackage, getPkgData, getPkgDataList, isLibrary, stagePackages,
    builderPath, getBuilderPath, isSpecified, latestBuildStage, programPath,
    programContext, integerLibraryName, destDir, stage1Only, buildDll0
    ) where

import Context
import CommandLine
import Expression
import Flavour
import GHC
import {-# SOURCE #-} Settings.Default
import Settings.Flavours.Development
import Settings.Flavours.Performance
import Settings.Flavours.Profiled
import Settings.Flavours.Quick
import Settings.Flavours.Quickest
import UserSettings

getArgs :: Args
getArgs = expr flavour >>= args

getLibraryWays :: Ways
getLibraryWays = expr flavour >>= libraryWays

getRtsWays :: Ways
getRtsWays = expr flavour >>= rtsWays

getPackages :: Packages
getPackages = expr flavour >>= packages

stagePackages :: Stage -> Action [Package]
stagePackages stage = interpretInContext (stageContext stage) getPackages

hadrianFlavours :: [Flavour]
hadrianFlavours =
    [ defaultFlavour, developmentFlavour Stage1, developmentFlavour Stage2
    , performanceFlavour, profiledFlavour, quickFlavour, quickestFlavour ]

flavour :: Action Flavour
flavour = do
    flavourName <- fromMaybe "default" <$> cmdFlavour
    let unknownFlavour = error $ "Unknown build flavour: " ++ flavourName
        flavours       = hadrianFlavours ++ userFlavours
    return $ fromMaybe unknownFlavour $ find ((== flavourName) . name) flavours

integerLibraryName :: Action String
integerLibraryName = pkgName <$> (integerLibrary =<< flavour)

programContext :: Stage -> Package -> Action Context
programContext stage pkg = do
    profiled <- ghcProfiled <$> flavour
    return $ if pkg == ghc && profiled && stage > Stage0
             then Context stage pkg profiling
             else vanillaContext stage pkg

-- TODO: switch to Set Package as the order of packages should not matter?
-- Otherwise we have to keep remembering to sort packages from time to time.
knownPackages :: [Package]
knownPackages = sort $ defaultKnownPackages ++ userKnownPackages

-- TODO: Speed up? Switch to Set?
-- Note: this is slow but we keep it simple as there are just ~50 packages
findKnownPackage :: PackageName -> Maybe Package
findKnownPackage name = find (\pkg -> pkgName pkg == name) knownPackages

-- | Determine the location of a 'Builder'.
builderPath :: Builder -> Action FilePath
builderPath builder = case builderProvenance builder of
    Nothing      -> systemBuilderPath builder
    Just context -> do
        maybePath <- programPath context
        let msg = error $ show builder ++ " is never built by Hadrian."
        return $ fromMaybe msg maybePath

getBuilderPath :: Builder -> Expr FilePath
getBuilderPath = expr . builderPath

-- | Was the path to a given 'Builder' specified in configuration files?
isSpecified :: Builder -> Action Bool
isSpecified = fmap (not . null) . builderPath

-- | Determine the latest 'Stage' in which a given 'Package' is built. Returns
-- Nothing if the package is never built.
latestBuildStage :: Package -> Action (Maybe Stage)
latestBuildStage pkg = do
    stages <- filterM (fmap (pkg `elem`) . stagePackages) [Stage0 ..]
    return $ if null stages then Nothing else Just $ maximum stages

-- | The 'FilePath' to a program executable in a given 'Context'.
programPath :: Context -> Action (Maybe FilePath)
programPath context@Context {..} = do
    maybeLatest <- latestBuildStage package
    path        <- buildPath context
    return $ do
        install <- (\l -> l == stage || package == ghc) <$> maybeLatest
        let installPath = if install then inplaceInstallPath package else path
        return $ installPath -/- programName context <.> exe

-- TODO: Set this from command line
-- | Stage1Only flag.
stage1Only :: Bool
stage1Only = defaultStage1Only

-- TODO: Set this from command line
-- | Install's DESTDIR setting.
destDir :: FilePath
destDir = defaultDestDir
