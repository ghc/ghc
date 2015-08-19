module Settings.Util (
    -- Primitive settings elements
    arg, argM,
    argSetting, argSettingList,
    getFlag, getSetting, getSettingList,
    getPkgData, getPkgDataList,
    getPackagePath, getTargetDirectory, getTargetPath, getHaddockPath,
    getPackageSources,
    appendCcArgs,
    needBuilder
    -- argBuilderPath, argStagedBuilderPath,
    -- argPackageKey, argPackageDeps, argPackageDepKeys, argSrcDirs,
    -- argIncludeDirs, argDepIncludeDirs,
    -- argConcat, argConcatPath, argConcatSpace,
    -- argPairs, argPrefix, argPrefixPath,
    -- argPackageConstraints,
    ) where

import Base
import Util
import Builder
import Package
import Expression
import Oracles.Flag
import Oracles.Setting
import Oracles.PackageData
import Settings.User
import Settings.TargetDirectory
import Data.List
import Data.Function

-- A single argument.
arg :: String -> Args
arg = append . return

argM :: Action String -> Args
argM = appendM . fmap return

argSetting :: Setting -> Args
argSetting = argM . setting

argSettingList :: SettingList -> Args
argSettingList = appendM . settingList

getFlag :: Flag -> Expr Bool
getFlag = lift . flag

getSetting :: Setting -> Expr String
getSetting = lift . setting

getSettingList :: SettingList -> Expr [String]
getSettingList = lift . settingList

getPkgData :: (FilePath -> PackageData) -> Expr String
getPkgData key = do
    stage <- getStage
    pkg   <- getPackage
    lift . pkgData . key $ targetPath stage pkg

getPkgDataList :: (FilePath -> PackageDataList) -> Expr [String]
getPkgDataList key = do
    stage <- getStage
    pkg   <- getPackage
    lift . pkgDataList . key $ targetPath stage pkg

getPackagePath :: Expr FilePath
getPackagePath = liftM pkgPath getPackage

getTargetDirectory :: Expr FilePath
getTargetDirectory = liftM2 targetDirectory getStage getPackage

getTargetPath :: Expr FilePath
getTargetPath = liftM2 targetPath getStage getPackage

getHaddockPath :: Expr FilePath
getHaddockPath = liftM pkgHaddockPath getPackage

-- Find all Haskell source files for the current target
getPackageSources :: Expr [FilePath]
getPackageSources = do
    path    <- getTargetPath
    pkgPath <- getPackagePath
    srcDirs <- getPkgDataList SrcDirs

    let buildPath = path -/- "build"
        dirs      = (buildPath -/- "autogen") : map (pkgPath -/-) srcDirs

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
    stage <- getStage
    mconcat [ builder (Gcc stage)  ? append xs
            , builder (GccM stage) ? append xs
            , builder GhcCabal     ? appendSub "--configure-option=CFLAGS" xs
            , builder GhcCabal     ? appendSub "--gcc-options" xs ]

-- Make sure a builder exists on the given path and rebuild it if out of date.
-- If laxDependencies is true (Settings/User.hs) then we do not rebuild GHC
-- even if it is out of date (can save a lot of build time when changing GHC).
needBuilder :: Builder -> Action ()
needBuilder ghc @ (Ghc stage) = do
    path <- builderPath ghc
    if laxDependencies then orderOnly [path] else need [path]

needBuilder builder = do
    path <- builderPath builder
    need [path]
