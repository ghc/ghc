module Settings (
    getArgs, getPackages, getLibraryWays, getRtsWays, flavour, knownPackages,
    findKnownPackage, getPkgData, getPkgDataList, isLibrary, getPackagePath,
    getContextDirectory, getBuildPath
    ) where

import Base
import CmdLineFlag
import Expression
import Flavour
import GHC
import Oracles.PackageData
import {-# SOURCE #-} Settings.Default
import Settings.Flavours.Quick
import Settings.Flavours.Quickest
import Settings.Paths
import UserSettings

getArgs :: Expr [String]
getArgs = fromDiffExpr $ args flavour

getLibraryWays :: Expr [Way]
getLibraryWays = fromDiffExpr $ libraryWays flavour

getRtsWays :: Expr [Way]
getRtsWays = fromDiffExpr $ rtsWays flavour

getPackages :: Expr [Package]
getPackages = fromDiffExpr $ packages flavour

getPackagePath :: Expr FilePath
getPackagePath = pkgPath <$> getPackage

getContextDirectory :: Expr FilePath
getContextDirectory = contextDirectory <$> getContext

getBuildPath :: Expr FilePath
getBuildPath = buildPath <$> getContext

getPkgData :: (FilePath -> PackageData) -> Expr String
getPkgData key = lift . pkgData . key =<< getBuildPath

getPkgDataList :: (FilePath -> PackageDataList) -> Expr [String]
getPkgDataList key = lift . pkgDataList . key =<< getBuildPath

hadrianFlavours :: [Flavour]
hadrianFlavours = [defaultFlavour, quickFlavour, quickestFlavour]

flavour :: Flavour
flavour = fromMaybe unknownFlavour $ find ((== flavourName) . name) flavours
  where
    unknownFlavour = error $ "Unknown build flavour: " ++ flavourName
    flavours       = hadrianFlavours ++ userFlavours
    flavourName    = fromMaybe "default" cmdFlavour

-- TODO: switch to Set Package as the order of packages should not matter?
-- Otherwise we have to keep remembering to sort packages from time to time.
knownPackages :: [Package]
knownPackages = sort $ defaultKnownPackages ++ userKnownPackages

-- TODO: Speed up?
-- Note: this is slow but we keep it simple as there are just ~50 packages
findKnownPackage :: PackageName -> Maybe Package
findKnownPackage name = find (\pkg -> pkgName pkg == name) knownPackages

-- TODO: add src-hc-args = -H32m -O
-- TODO: GhcStage2HcOpts=-O2 unless GhcUnregisterised
-- TODO: compiler/stage1/build/Parser_HC_OPTS += -O0 -fno-ignore-interface-pragmas
-- TODO: compiler/main/GhcMake_HC_OPTS        += -auto-all
-- TODO: compiler/prelude/PrimOp_HC_OPTS  += -fforce-recomp
-- TODO: is GhcHcOpts=-Rghc-timing needed?
