{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Oracles.ModuleFiles (moduleFiles, haskellModuleFiles, moduleFilesOracle) where

import Base
import Context
import Oracles.PackageData
import Package
import Settings.Paths

newtype ModuleFilesKey = ModuleFilesKey ([FilePath], [String])
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

moduleFiles :: Context -> Action [FilePath]
moduleFiles context @ (Context {..}) = do
    let path = contextPath context
    srcDirs <- fmap sort . pkgDataList $ SrcDirs path
    modules <- fmap sort . pkgDataList $ Modules path
    let dirs = [ pkgPath package -/- dir | dir <- srcDirs ]
    found :: [(String, FilePath)] <- askOracle $ ModuleFilesKey (dirs, modules)
    return $ map snd found

haskellModuleFiles :: Context -> Action ([FilePath], [String])
haskellModuleFiles context @ (Context {..}) = do
    let path        = contextPath context
        autogen     = path -/- "build/autogen"
        dropPkgPath = drop $ length (pkgPath package) + 1
    srcDirs <- fmap sort . pkgDataList $ SrcDirs path
    modules <- fmap sort . pkgDataList $ Modules path
    let dirs = [ pkgPath package -/- dir | dir <- srcDirs ]
    foundSrcDirs <- askOracle $ ModuleFilesKey (dirs     , modules)
    foundAutogen <- askOracle $ ModuleFilesKey ([autogen], modules)

    let found          = foundSrcDirs ++ foundAutogen
        missingMods    = modules `minusOrd` (sort $ map fst found)
        otherFileToMod = replaceEq '/' '.' . dropExtension . dropPkgPath
        (haskellFiles, otherFiles) = partition ("//*hs" ?==) (map snd found)

    return (haskellFiles, missingMods ++ map otherFileToMod otherFiles)

-- | This is an important oracle whose role is to find and cache module source
-- files. More specifically, it takes a list of directories @dirs@ and a sorted
-- list of module names @modules@ as arguments, and for each module, e.g.
-- @A.B.C@, returns a FilePath of the form @dir/A/B/C.extension@, such that
-- @dir@ belongs to @dirs@, and file @dir/A/B/C.extension@ exists, or Nothing
-- if there is no such file. If more than one matching file is found an error is
-- raised. For example, for the 'compiler' package given
-- @dirs = ["codeGen", "parser"]@, and
-- @modules = ["CodeGen.Platform.ARM", "Lexer", "Missing.Module"]@, it produces
-- @[Just "codeGen/CodeGen/Platform/ARM.hs", Just "parser/Lexer.x", Nothing]@.
moduleFilesOracle :: Rules ()
moduleFilesOracle = void $
    addOracle $ \(ModuleFilesKey (dirs, modules)) -> do
        let decodedPairs = map decodeModule modules
            modDirFiles  = map (bimap head sort . unzip)
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

        return $ sort [ (encodeModule d f, f) | (fs, d) <- result, f <- fs ]
