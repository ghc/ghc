module Settings (
    getArgs, getPackages, getLibraryWays, getRtsWays, flavour, knownPackages,
    findKnownPackage, getPkgData, getPkgDataList, isLibrary, getPackagePath,
    getContextDirectory, getBuildPath, stagePackages, builderPath,
    getBuilderPath, isSpecified, latestBuildStage, programPath, programContext,
    integerLibraryName
    ) where

import Base
import Context
import CmdLineFlag
import Expression
import Flavour
import GHC
import Oracles.PackageData
import Oracles.Path
import {-# SOURCE #-} Settings.Default
import Settings.Flavours.Development
import Settings.Flavours.Perf
import Settings.Flavours.Prof
import Settings.Flavours.Quick
import Settings.Flavours.Quickest
import Settings.Path
import UserSettings

getArgs :: Expr [String]
getArgs = fromDiffExpr $ args flavour

getLibraryWays :: Expr [Way]
getLibraryWays = fromDiffExpr $ libraryWays flavour

getRtsWays :: Expr [Way]
getRtsWays = fromDiffExpr $ rtsWays flavour

getPackages :: Expr [Package]
getPackages = fromDiffExpr $ packages flavour

stagePackages :: Stage -> Action [Package]
stagePackages stage = interpretInContext (stageContext stage) getPackages

getPackagePath :: Expr FilePath
getPackagePath = pkgPath <$> getPackage

getContextDirectory :: Expr FilePath
getContextDirectory = stageDirectory <$> getStage

getBuildPath :: Expr FilePath
getBuildPath = buildPath <$> getContext

getPkgData :: (FilePath -> PackageData) -> Expr String
getPkgData key = lift . pkgData . key =<< getBuildPath

getPkgDataList :: (FilePath -> PackageDataList) -> Expr [String]
getPkgDataList key = lift . pkgDataList . key =<< getBuildPath

hadrianFlavours :: [Flavour]
hadrianFlavours =
    [ defaultFlavour, developmentFlavour Stage1, developmentFlavour Stage2
    , perfFlavour, profFlavour, quickFlavour, quickestFlavour ]

flavour :: Flavour
flavour = fromMaybe unknownFlavour $ find ((== flavourName) . name) flavours
  where
    unknownFlavour = error $ "Unknown build flavour: " ++ flavourName
    flavours       = hadrianFlavours ++ userFlavours
    flavourName    = fromMaybe "default" cmdFlavour

integerLibraryName :: String
integerLibraryName = pkgNameString $ integerLibrary flavour

programContext :: Stage -> Package -> Context
programContext stage pkg
    | pkg == ghc && ghcProfiled flavour = Context stage pkg profiling
    | otherwise = vanillaContext stage pkg

-- TODO: switch to Set Package as the order of packages should not matter?
-- Otherwise we have to keep remembering to sort packages from time to time.
knownPackages :: [Package]
knownPackages = sort $ defaultKnownPackages ++ userKnownPackages

-- TODO: Speed up?
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

getBuilderPath :: Builder -> ReaderT a Action FilePath
getBuilderPath = lift . builderPath

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
    return $ do
        install <- (\l -> l == stage || package == ghc) <$> maybeLatest
        let path = if install then installPath package else buildPath context
        return $ path -/- programName context <.> exe
