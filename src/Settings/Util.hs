module Settings.Util (
    module Settings.TargetDirectory,
    arg, argM,
    argSetting, argSettingList, argStagedSettingList, argStagedBuilderPath,
    getFlag, getSetting, getSettingList, getStagedSettingList,
    getPkgData, getPkgDataList,
    getPackagePath, getTargetDirectory, getTargetPath, getHaddockFile,
    getPackageSources,
    appendCcArgs
    ) where

import Base
import Stage
import Builder
import Package
import Expression
import Predicates
import Oracles
import Settings.TargetDirectory

-- A single argument.
arg :: String -> Args
arg = append . return

argM :: Action String -> Args
argM = (arg =<<) . lift

argSetting :: Setting -> Args
argSetting = argM . setting

argSettingList :: SettingList -> Args
argSettingList = appendM . settingList

argStagedSettingList :: (Stage -> SettingList) -> Args
argStagedSettingList ss = (argSettingList . ss) =<< getStage

argStagedBuilderPath :: (Stage -> Builder) -> Args
argStagedBuilderPath sb = (argM . builderPath . sb) =<< getStage

getFlag :: Flag -> Expr Bool
getFlag = lift . flag

getSetting :: Setting -> Expr String
getSetting = lift . setting

getSettingList :: SettingList -> Expr [String]
getSettingList = lift . settingList

getStagedSettingList :: (Stage -> SettingList) -> Expr [String]
getStagedSettingList ss = lift . settingList . ss =<< getStage

getPkgData :: (FilePath -> PackageData) -> Expr String
getPkgData key = lift . pkgData . key =<< getTargetPath

getPkgDataList :: (FilePath -> PackageDataList) -> Expr [String]
getPkgDataList key = lift . pkgDataList . key =<< getTargetPath

getPackagePath :: Expr FilePath
getPackagePath = liftM pkgPath getPackage

getTargetDirectory :: Expr FilePath
getTargetDirectory = liftM2 targetDirectory getStage getPackage

getTargetPath :: Expr FilePath
getTargetPath = liftM2 targetPath getStage getPackage

getHaddockFile :: Expr FilePath
getHaddockFile = liftM pkgHaddockFile getPackage

-- Find all Haskell source files for the current target
getPackageSources :: Expr [FilePath]
getPackageSources = do
    path        <- getTargetPath
    packagePath <- getPackagePath
    srcDirs     <- getPkgDataList SrcDirs

    let buildPath = path -/- "build"
        dirs      = (buildPath -/- "autogen") : map (packagePath -/-) srcDirs

    (foundSources, missingSources) <- findModuleFiles dirs "*hs"

    -- Generated source files live in buildPath and have extension "hs"
    let generatedSources = [ buildPath -/- s <.> "hs" | s <- missingSources ]

    return $ foundSources ++ generatedSources

-- findModuleFiles scans a list of given directories and finds files matching a
-- given extension pattern (e.g., "*hs") that correspond to modules of the
-- currently built package. Missing module files are returned in a separate
-- list. The returned pair contains the following:
-- * a list of found module files, with paths being relative to one of given
--   directories, e.g. "codeGen/CodeGen/Platform.hs" for the compiler package.
-- * a list of module files that have not been found, with paths being relative
--   to the module directory, e.g. "CodeGen/Platform", and with no extension.
findModuleFiles :: [FilePath] -> FilePattern -> Expr ([FilePath], [FilePath])
findModuleFiles dirs extension = do
    modules <- getPkgDataList Modules
    let decodedMods    = sort . map decodeModule $ modules
        modDirFiles    = map (bimap head sort . unzip)
                       . groupBy ((==) `on` fst) $ decodedMods
        matchExtension = (?==) ("*" <.> extension)

    result <- lift . fmap concat . forM dirs $ \dir -> do
        todo <- filterM (doesDirectoryExist . (dir -/-) . fst) modDirFiles
        forM todo $ \(mDir, mFiles) -> do
            let fullDir = dir -/- mDir
            files <- fmap (filter matchExtension) $ getDirectoryContents fullDir
            let cmp fe f = compare (dropExtension fe) f
                found    = intersectOrd cmp files mFiles
            return (map (fullDir -/-) found, (mDir, map dropExtension found))

    let foundFiles   = concatMap fst result
        foundMods    = [ (d, f) | (d, fs) <- map snd result, f <- fs ]
        missingMods  = decodedMods `minusOrd` sort foundMods
        missingFiles = map (uncurry (-/-)) missingMods

    return (foundFiles, missingFiles)

-- Pass arguments to Gcc and corresponding lists of sub-arguments of GhcCabal
appendCcArgs :: [String] -> Args
appendCcArgs xs = do
    mconcat [ stagedBuilder Gcc  ? append xs
            , stagedBuilder GccM ? append xs
            , builder GhcCabal   ? appendSub "--configure-option=CFLAGS" xs
            , builder GhcCabal   ? appendSub "--gcc-options" xs ]

