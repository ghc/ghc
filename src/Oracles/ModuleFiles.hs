{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Oracles.ModuleFiles (
    moduleFiles, haskellSources, moduleFilesOracle, findModuleFiles
    ) where

import Base
import Context
import Expression
import Oracles.PackageData
import Settings.Paths

newtype ModuleFilesKey = ModuleFilesKey ([FilePath], [String])
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

moduleFiles :: Context -> Action [FilePath]
moduleFiles context @ Context {..} = do
    let path    = contextPath context
        autogen = path -/- "build/autogen"
    srcDirs <- fmap sort . pkgDataList $ SrcDirs path
    modules <- fmap sort . pkgDataList $ Modules path
    let dirs = [ pkgPath package -/- dir | dir <- srcDirs ]
    catMaybes <$> findModuleFiles (autogen : dirs) modules

haskellModuleFiles :: Context -> Action ([FilePath], [String])
haskellModuleFiles context @ Context {..} = do
    let path        = contextPath context
        autogen     = path -/- "build/autogen"
        dropPkgPath = drop $ length (pkgPath package) + 1
    srcDirs <- fmap sort . pkgDataList $ SrcDirs path
    modules <- fmap sort . pkgDataList $ Modules path
    let dirs = [ pkgPath package -/- dir | dir <- srcDirs ]
    found <- findModuleFiles (autogen : dirs) modules
    let missingMods    = map fst . filter (isNothing . snd) $ zip modules found
        otherFileToMod = replaceEq '/' '.' . dropExtension . dropPkgPath
        (haskellFiles, otherFiles) = partition ("//*hs" ?==) $ catMaybes found
    return (haskellFiles, missingMods ++ map otherFileToMod otherFiles)

-- | Find all Haskell source files for the current context
haskellSources :: Context -> Action [FilePath]
haskellSources context = do
    let buildPath = contextPath context -/- "build"
        autogen   = buildPath -/- "autogen"
    (found, missingMods) <- haskellModuleFiles context
    -- Generated source files live in buildPath and have extension "hs"...
    let generated = [ buildPath -/- (replaceEq '.' '/' m) <.> "hs" | m <- missingMods ]
    -- ...except that GHC/Prim.hs lives in autogen. TODO: fix the inconsistency?
        fixGhcPrim = replaceEq (buildPath -/- "GHC/Prim.hs") (autogen -/- "GHC/Prim.hs")
    return $ found ++ fixGhcPrim generated

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
