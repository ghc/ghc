module Settings (
    module Settings.Packages,
    module Settings.Paths,
    module Settings.User,
    module Settings.Ways,
    getPkgData, getPkgDataList, getTopDirectory, isLibrary,
    getPackagePath, getContextDirectory, getContextPath
    ) where

import Base
import Expression
import Oracles.PackageData
import Oracles.WindowsPath
import Settings.Packages
import Settings.Paths
import Settings.User
import Settings.Ways

getPackagePath :: Expr FilePath
getPackagePath = pkgPath <$> getPackage

getContextDirectory :: Expr FilePath
getContextDirectory = contextDirectory <$> getContext

getContextPath :: Expr FilePath
getContextPath = contextPath <$> getContext

getPkgData :: (FilePath -> PackageData) -> Expr String
getPkgData key = lift . pkgData . key =<< getContextPath

getPkgDataList :: (FilePath -> PackageDataList) -> Expr [String]
getPkgDataList key = lift . pkgDataList . key =<< getContextPath

getTopDirectory :: Expr FilePath
getTopDirectory = lift topDirectory
