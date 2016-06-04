module Settings (
    module Settings.Packages,
    module Settings.Paths,
    module Settings.Ways,
    module UserSettings,
    getPkgData, getPkgDataList, getTopDirectory, isLibrary,
    getPackagePath, getContextDirectory, getBuildPath
    ) where

import Base
import Expression
import Oracles.PackageData
import Oracles.WindowsPath
import Settings.Packages
import Settings.Paths
import Settings.Ways
import UserSettings

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

getTopDirectory :: Expr FilePath
getTopDirectory = lift topDirectory
