{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Oracles.ModuleFiles (moduleFiles, haskellModuleFiles, moduleFilesOracle) where

import Base
import Oracles.PackageData
import Package
import Stage
import Settings.Paths

newtype ModuleFilesKey = ModuleFilesKey ([String], [FilePath])
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

moduleFiles :: Stage -> Package -> Action [FilePath]
moduleFiles stage pkg = do
    let path = targetPath stage pkg
    srcDirs <- fmap sort . pkgDataList $ SrcDirs path
    modules <- fmap sort . pkgDataList $ Modules path
    let dirs = [ pkgPath pkg -/- dir | dir <- srcDirs ]
    found :: [(String, FilePath)] <- askOracle $ ModuleFilesKey (modules, dirs)
    return $ map snd found

haskellModuleFiles :: Stage -> Package -> Action ([FilePath], [String])
haskellModuleFiles stage pkg = do
    let path        = targetPath stage pkg
        autogen     = path -/- "build/autogen"
        dropPkgPath = drop $ length (pkgPath pkg) + 1
    srcDirs <- fmap sort . pkgDataList $ SrcDirs path
    modules <- fmap sort . pkgDataList $ Modules path
    let dirs = [ pkgPath pkg -/- dir | dir <- srcDirs ]
    foundSrcDirs <- askOracle $ ModuleFilesKey (modules, dirs     )
    foundAutogen <- askOracle $ ModuleFilesKey (modules, [autogen])

    let found          = foundSrcDirs ++ foundAutogen
        missingMods    = modules `minusOrd` (sort $ map fst found)
        otherFileToMod = replaceEq '/' '.' . dropExtension . dropPkgPath
        (haskellFiles, otherFiles) = partition ("//*hs" ?==) (map snd found)

    return (haskellFiles, missingMods ++ map otherFileToMod otherFiles)

-- | This is an important oracle whose role is to find and cache module source
-- files. More specifically, it takes a list of module names @modules@ and a
-- list of directories @dirs@ as arguments, and computes a sorted list of pairs
-- of the form @(A.B.C, dir/A/B/C.extension)@, such that @A.B.C@ belongs to
-- @modules@, @dir@ belongs to @dirs@, and file @dir/A/B/C.extension@ exists.
-- For example, for 'compiler' package given
-- @modules = ["CodeGen.Platform.ARM", "Lexer"]@, and
-- @dirs = ["codeGen", "parser"]@, it produces
-- @[("CodeGen.Platform.ARM", "codeGen/CodeGen/Platform/ARM.hs"),
-- ("Lexer", "parser/Lexer.x")]@.
moduleFilesOracle :: Rules ()
moduleFilesOracle = void $
    addOracle $ \(ModuleFilesKey (modules, dirs)) -> do
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
