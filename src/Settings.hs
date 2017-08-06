module Settings (
    getArgs, getPackages, getLibraryWays, getRtsWays, flavour, knownPackages,
    findKnownPackage, getPkgData, getPkgDataList, isLibrary, getPackagePath,
    getContextDirectory, getBuildPath, stagePackages, builderPath,
    getBuilderPath, isSpecified, latestBuildStage, programPath, programContext,
    integerLibraryName, destDir, pkgConfInstallPath, stage1Only
    ) where

import Hadrian.Oracles.Path

import Base
import Context
import CmdLineFlag
import Expression
import Flavour
import GHC
import Oracles.Config
import Oracles.PackageData
import {-# SOURCE #-} Settings.Default
import Settings.Flavours.Development
import Settings.Flavours.Performance
import Settings.Flavours.Profiled
import Settings.Flavours.Quick
import Settings.Flavours.Quickest
import Settings.Path
import UserSettings

getArgs :: Expr [String]
getArgs = args flavour

getLibraryWays :: Expr [Way]
getLibraryWays = libraryWays flavour

getRtsWays :: Expr [Way]
getRtsWays = rtsWays flavour

getPackages :: Expr [Package]
getPackages = packages flavour

stagePackages :: Stage -> Action [Package]
stagePackages stage = interpretInContext (stageContext stage) getPackages

getPackagePath :: Expr FilePath
getPackagePath = pkgPath <$> getPackage

getContextDirectory :: Expr FilePath
getContextDirectory = stageDirectory <$> getStage

getBuildPath :: Expr FilePath
getBuildPath = buildPath <$> getContext

getPkgData :: (FilePath -> PackageData) -> Expr String
getPkgData key = expr . pkgData . key =<< getBuildPath

getPkgDataList :: (FilePath -> PackageDataList) -> Expr [String]
getPkgDataList key = expr . pkgDataList . key =<< getBuildPath

hadrianFlavours :: [Flavour]
hadrianFlavours =
    [ defaultFlavour, developmentFlavour Stage1, developmentFlavour Stage2
    , performanceFlavour, profiledFlavour, quickFlavour, quickestFlavour ]

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
    | pkg == ghc && ghcProfiled flavour && stage > Stage0 = Context stage pkg profiling
    | otherwise = vanillaContext stage pkg

-- TODO: switch to Set Package as the order of packages should not matter?
-- Otherwise we have to keep remembering to sort packages from time to time.
knownPackages :: [Package]
knownPackages = sort $ defaultKnownPackages ++ userKnownPackages

-- TODO: Speed up?
-- Note: this is slow but we keep it simple as there are just ~50 packages
findKnownPackage :: PackageName -> Maybe Package
findKnownPackage name = find (\pkg -> pkgName pkg == name) knownPackages

-- | Determine the location of a system 'Builder'.
systemBuilderPath :: Builder -> Action FilePath
systemBuilderPath builder = case builder of
    Alex            -> fromKey "alex"
    Ar Stage0       -> fromKey "system-ar"
    Ar _            -> fromKey "ar"
    Cc  _  Stage0   -> fromKey "system-cc"
    Cc  _  _        -> fromKey "cc"
    -- We can't ask configure for the path to configure!
    Configure _     -> return "sh configure"
    Ghc _  Stage0   -> fromKey "system-ghc"
    GhcPkg _ Stage0 -> fromKey "system-ghc-pkg"
    Happy           -> fromKey "happy"
    HsColour        -> fromKey "hscolour"
    HsCpp           -> fromKey "hs-cpp"
    Ld              -> fromKey "ld"
    Make _          -> fromKey "make"
    Nm              -> fromKey "nm"
    Objdump         -> fromKey "objdump"
    Patch           -> fromKey "patch"
    Perl            -> fromKey "perl"
    Ranlib          -> fromKey "ranlib"
    Tar             -> fromKey "tar"
    _               -> error $ "No system.config entry for " ++ show builder
  where
    fromKey key = do
        let unpack = fromMaybe . error $ "Cannot find path to builder "
                ++ quote key ++ " in system.config file. Did you skip configure?"
        path <- unpack <$> askConfig key
        if null path
        then do
            unless (isOptional builder) . error $ "Non optional builder "
                ++ quote key ++ " is not specified in system.config file."
            return "" -- TODO: Use a safe interface.
        else fixAbsolutePathOnWindows =<< lookupInPath path

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
    return $ do
        install <- (\l -> l == stage || package == ghc) <$> maybeLatest
        let path = if install then inplaceInstallPath package else buildPath context
        return $ path -/- programName context <.> exe

pkgConfInstallPath :: FilePath
pkgConfInstallPath = buildPath (vanillaContext Stage0 rts) -/- "package.conf.install"

-- | Stage1Only flag
-- TODO: Set this by cmdline flags
stage1Only :: Bool
stage1Only = defaultStage1Only

-- | Install's DESTDIR flag
-- TODO: Set this by cmdline flags
destDir :: FilePath
destDir = defaultDestDir
