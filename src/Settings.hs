module Settings (
    module Settings.Packages,
    module Settings.TargetDirectory,
    module Settings.User,
    module Settings.Ways,
    getPkgData, getPkgDataList, programPath, isLibrary,
    getPackagePath, getTargetDirectory, getTargetPath, getPackageSources
    ) where

import Expression
import Oracles
import Oracles.ModuleFiles
import Settings.Packages
import Settings.TargetDirectory
import Settings.User
import Settings.Ways

getPackagePath :: Expr FilePath
getPackagePath = liftM pkgPath getPackage

getTargetDirectory :: Expr FilePath
getTargetDirectory = liftM2 targetDirectory getStage getPackage

getTargetPath :: Expr FilePath
getTargetPath = liftM2 targetPath getStage getPackage

getPkgData :: (FilePath -> PackageData) -> Expr String
getPkgData key = lift . pkgData . key =<< getTargetPath

getPkgDataList :: (FilePath -> PackageDataList) -> Expr [String]
getPkgDataList key = lift . pkgDataList . key =<< getTargetPath

programPath :: Stage -> Package -> Maybe FilePath
programPath = userProgramPath

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
