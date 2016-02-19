module Settings (
    module Settings.Packages,
    module Settings.Paths,
    module Settings.User,
    module Settings.Ways,
    getPkgData, getPkgDataList, getTopDirectory, isLibrary,
    getPackagePath, getContextDirectory, getContextPath, getPackageSources
    ) where

import Base
import Expression
import Oracles.ModuleFiles
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

-- | Find all Haskell source files for the current target
getPackageSources :: Expr [FilePath]
getPackageSources = do
    context <- getContext
    let buildPath = contextPath context -/- "build"
        autogen   = buildPath -/- "autogen"
    (found, missingMods) <- lift $ haskellModuleFiles context
    -- Generated source files live in buildPath and have extension "hs"...
    let generated = [ buildPath -/- (replaceEq '.' '/' m) <.> "hs" | m <- missingMods ]
    -- ...except that GHC/Prim.hs lives in autogen. TODO: fix the inconsistency?
        fixGhcPrim = replaceEq (buildPath -/- "GHC/Prim.hs") (autogen -/- "GHC/Prim.hs")
    return $ found ++ fixGhcPrim generated
