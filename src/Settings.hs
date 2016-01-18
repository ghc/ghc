module Settings (
    module Settings.Packages,
    module Settings.Paths,
    module Settings.User,
    module Settings.Ways,
    getPkgData, getPkgDataList, getTopDirectory, isLibrary,
    getPackagePath, getTargetDirectory, getTargetPath, getPackageSources
    ) where

import Base
import Expression
import Oracles
import Oracles.ModuleFiles
import Settings.Packages
import Settings.Paths
import Settings.User
import Settings.Ways

getPackagePath :: Expr FilePath
getPackagePath = pkgPath <$> getPackage

getTargetDirectory :: Expr FilePath
getTargetDirectory = targetDirectory <$> getStage <*> getPackage

getTargetPath :: Expr FilePath
getTargetPath = targetPath <$> getStage <*> getPackage

getPkgData :: (FilePath -> PackageData) -> Expr String
getPkgData key = lift . pkgData . key =<< getTargetPath

getPkgDataList :: (FilePath -> PackageDataList) -> Expr [String]
getPkgDataList key = lift . pkgDataList . key =<< getTargetPath

getTopDirectory :: Expr FilePath
getTopDirectory = lift topDirectory

-- | Find all Haskell source files for the current target
getPackageSources :: Expr [FilePath]
getPackageSources = do
    stage <- getStage
    pkg   <- getPackage
    path  <- getTargetPath
    let buildPath = path -/- "build"
        autogen   = buildPath -/- "autogen"
    (found, missingMods) <- lift $ haskellModuleFiles stage pkg
    -- Generated source files live in buildPath and have extension "hs"...
    let generated = [ buildPath -/- (replaceEq '.' '/' m) <.> "hs" | m <- missingMods ]
    -- ...except that GHC/Prim.hs lives in autogen. TODO: fix the inconsistency?
        fixGhcPrim = replaceEq (buildPath -/- "GHC/Prim.hs") (autogen -/- "GHC/Prim.hs")
    return $ found ++ fixGhcPrim generated
