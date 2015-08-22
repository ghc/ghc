module Settings (
    module Settings.Packages,
    module Settings.TargetDirectory,
    module Settings.User,
    module Settings.Ways,
    getPkgData, getPkgDataList,
    getPackagePath, getTargetDirectory, getTargetPath, getPackageSources,
    ) where

import Expression
import Oracles
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

-- Find all Haskell source files for the current target. TODO: simplify.
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
