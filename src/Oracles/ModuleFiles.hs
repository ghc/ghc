{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Oracles.ModuleFiles (
    moduleFiles, haskellModuleFiles, moduleFilesOracle, findModuleFiles
    ) where

import Base
import Context
import Oracles.PackageData
import Package
import Settings.Paths

newtype ModuleFilesKey = ModuleFilesKey ([FilePath], [String])
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

moduleFiles :: Context -> Action [FilePath]
moduleFiles context @ Context {..} = do
    let path = contextPath context
    srcDirs <- fmap sort . pkgDataList $ SrcDirs path
    modules <- fmap sort . pkgDataList $ Modules path
    let dirs = [ pkgPath package -/- dir | dir <- srcDirs ]
    fmap catMaybes $ findModuleFiles dirs modules

haskellModuleFiles :: Context -> Action ([FilePath], [String])
haskellModuleFiles context @ Context {..} = do
    let path        = contextPath context
        autogen     = path -/- "build/autogen"
        dropPkgPath = drop $ length (pkgPath package) + 1
    srcDirs <- fmap sort . pkgDataList $ SrcDirs path
    modules <- fmap sort . pkgDataList $ Modules path
    let dirs = [ pkgPath package -/- dir | dir <- srcDirs ]
    foundSrcDirs <- findModuleFiles dirs      modules
    foundAutogen <- findModuleFiles [autogen] modules
    found <- sequence $ zipWith3 addSources modules foundSrcDirs foundAutogen

    let missingMods    = map fst . filter (isNothing . snd) $ zip modules found
        otherFileToMod = replaceEq '/' '.' . dropExtension . dropPkgPath
        (haskellFiles, otherFiles) = partition ("//*hs" ?==) $ catMaybes found

    return (haskellFiles, missingMods ++ map otherFileToMod otherFiles)
  where
    addSources _ Nothing   r         = return r
    addSources _ l         Nothing   = return l
    addSources m (Just f1) (Just f2) = errorMultipleSources m f1 f2

-- | This is an important oracle whose role is to find and cache module source
-- files. More specifically, it takes a list of directories @dirs@ and a sorted
-- list of module names @modules@ as arguments, and for each module, e.g.
-- @A.B.C@, returns a 'FilePath' of the form @dir/A/B/C.extension@, such that
-- @dir@ belongs to @dirs@, and file @dir/A/B/C.extension@ exists, or 'Nothing'
-- if there is no such file. If more than one matching file is found an error is
-- raised. For example, for the 'compiler' package given
-- @dirs = ["compiler/codeGen", "compiler/parser"]@, and
-- @modules = ["CodeGen.Platform.ARM", "Lexer", "Missing.Module"]@, it produces
-- @[Just "compiler/codeGen/CodeGen/Platform/ARM.hs",
-- Just "compiler/parser/Lexer.x", Nothing]@.
findModuleFiles :: [FilePath] -> [String] -> Action [Maybe FilePath]
findModuleFiles dirs modules = askOracle $ ModuleFilesKey (dirs, modules)

moduleFilesOracle :: Rules ()
moduleFilesOracle = void $
    addOracle $ \(ModuleFilesKey (dirs, modules)) -> do
        let decodedPairs = map decodeModule modules
            modDirFiles  = map (bimap head id . unzip)
                         . groupBy ((==) `on` fst) $ decodedPairs

        result <- fmap concat . forM dirs $ \dir -> do
            todo <- filterM (doesDirectoryExist . (dir -/-) . fst) modDirFiles
            forM todo $ \(mDir, mFiles) -> do
                let fullDir = dir -/- mDir
                files <- getDirectoryFiles fullDir ["*"]
                let noBoot   = filter (not . (isSuffixOf "-boot")) files
                    cmp fe f = compare (dropExtension fe) f
                    found    = intersectOrd cmp noBoot mFiles
                return (map (fullDir -/-) found, mDir)

        let pairs = sort [ (encodeModule d f, f) | (fs, d) <- result, f <- fs ]
            multi = [ (m, f1, f2) | (m, f1):(n, f2):_ <- tails pairs, m == n ]

        unless (null multi) $ do
            let (m, f1, f2) = head multi
            errorMultipleSources m f1 f2

        return $ lookupAll modules pairs

errorMultipleSources :: String -> FilePath -> FilePath -> Action a
errorMultipleSources m f1 f2 = putError $ "Module " ++ m ++
    " has more than one source file: " ++ f1 ++ " and " ++ f2 ++ "."
