{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Oracles.ModuleFiles (moduleFiles, haskellModuleFiles, moduleFilesOracle) where

import Base hiding (exe)
import Distribution.ModuleName
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import GHC
import Oracles.PackageData
import Package hiding (library)
import Stage
import Settings.TargetDirectory

newtype ModuleFilesKey = ModuleFilesKey (Package, [FilePath])
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

moduleFiles :: Stage -> Package -> Action [FilePath]
moduleFiles stage pkg = do
    let path = targetPath stage pkg
    modules <- fmap sort . pkgDataList $ Modules path
    (found, _ :: [FilePath]) <- askOracle $ ModuleFilesKey (pkg, [])
    let cmp (m1, _) m2 = compare m1 m2
        foundFiles     = map snd $ intersectOrd cmp found modules
    return foundFiles

haskellModuleFiles :: Stage -> Package -> Action ([FilePath], [String])
haskellModuleFiles stage pkg = do
    let path    = targetPath stage pkg
        autogen = path -/- "build/autogen"
    modules <- fmap sort . pkgDataList $ Modules path
    (found, missingMods) <- askOracle $ ModuleFilesKey (pkg, [autogen])
    let cmp (m1, _) m2 = compare m1 m2
        foundFiles     = map snd $ intersectOrd cmp found modules
        otherMods      = map (replaceEq '/' '.' . dropExtension) otherFiles
        (haskellFiles, otherFiles) = partition ("//*hs" ?==) foundFiles
    return (haskellFiles, missingMods ++ otherMods)

extract :: Monoid a => Maybe (CondTree v c a) -> a
extract Nothing = mempty
extract (Just (CondNode leaf _ ifs)) = leaf <> mconcat (map f ifs)
  where
    f (_, t, mt) = extract (Just t) <> extract mt

-- Look up Haskell source directories and module names of a package
packageInfo :: Package -> Action ([FilePath], [ModuleName])
packageInfo pkg
    | pkg == hp2ps = return (["."], [])
    | otherwise    = do
        need [pkgCabalFile pkg]
        pd <- liftIO . readPackageDescription silent $ pkgCabalFile pkg

        let lib = extract                     $ condLibrary     pd
            exe = extract . Just . snd . head $ condExecutables pd

        let (srcDirs, modules) = if lib /= mempty
                then ( hsSourceDirs $ libBuildInfo lib, libModules lib)
                else ( hsSourceDirs $    buildInfo exe
                     , [fromString . dropExtension $ modulePath exe]
                     ++ exeModules exe)

        return (if null srcDirs then ["."] else srcDirs, modules)

moduleFilesOracle :: Rules ()
moduleFilesOracle = do
    answer <- newCache $ \(pkg, extraDirs) -> do
        putOracle $ "Searching module files of package " ++ pkgNameString pkg ++ "..."
        unless (null extraDirs) $ putOracle $ "Extra directory = " ++ show extraDirs

        (srcDirs, modules) <- packageInfo pkg

        let dirs         = extraDirs ++ [ pkgPath pkg -/- dir | dir <- srcDirs ]
            decodedPairs = sort $ map (splitFileName . toFilePath) modules
            modDirFiles  = map (bimap head sort . unzip)
                         . groupBy ((==) `on` fst) $ decodedPairs

        result <- fmap concat . forM dirs $ \dir -> do
            todo <- filterM (doesDirectoryExist . (dir -/-) . fst) modDirFiles
            forM todo $ \(mDir, mFiles) -> do
                let fullDir = dir -/- mDir
                files <- getDirectoryFiles fullDir ["*"]
                let noBoot = filter (not . (isSuffixOf "-boot")) files
                    cmp fe f = compare (dropExtension fe) f
                    found    = intersectOrd cmp noBoot mFiles
                return (map (fullDir -/-) found, (mDir, map dropExtension found))

        let foundFiles   = sort [ (encodeModule d f, f)
                                | (fs, (d, _)) <- result, f <- fs ]
            foundPairs   = [ (d, f) | (d, fs) <- map snd result, f <- fs ]
            missingPairs = decodedPairs `minusOrd` sort foundPairs
            missingMods  = map (uncurry encodeModule) missingPairs

        return (foundFiles, missingMods)

    _ <- addOracle $ \(ModuleFilesKey query) -> answer query
    return ()
