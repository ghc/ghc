{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, LambdaCase #-}
module Oracles.ModuleFiles (
    findGenerator, haskellSources, moduleFilesOracle, findModuleFiles
    ) where

import qualified Data.HashMap.Strict as Map

import Base
import Context
import Expression
import Oracles.PackageData
import Settings.Paths

newtype ModuleFilesKey = ModuleFilesKey ([FilePath], [String])
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

newtype Generator = Generator (Context, FilePath)
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- The following generators and corresponding source extensions are supported:
determineBuilder :: FilePath -> Maybe Builder
determineBuilder file = case takeExtension file of
    ".x"   -> Just Alex
    ".y"   -> Just Happy
    ".ly"  -> Just Happy
    ".hsc" -> Just Hsc2Hs
    _      -> Nothing

-- | Find the generator for a given 'Context' and a source file. For example:
-- findGenerator (Context Stage1 compiler vanilla)
--               ".build/stage1/compiler/build/Lexer.hs"
-- == Just ("compiler/parser/Lexer.x", Alex)
-- findGenerator (Context Stage1 base vanilla)
--               ".build/stage1/base/build/Prelude.hs"
-- == Nothing
findGenerator :: Context -> FilePath -> Action (Maybe (FilePath, Builder))
findGenerator context file = askOracle $ Generator (context, file)

-- | Find all Haskell source files for a given 'Context'.
haskellSources :: Context -> Action [FilePath]
haskellSources context = do
    let autogen = contextPath context -/- "build/autogen"
    -- Generated source files live in build/ and have extension "hs", except
    -- that GHC/Prim.hs lives in build/autogen/. TODO: fix the inconsistency?
    let modFile ("GHC.Prim", _) = autogen -/- "GHC/Prim.hs"
        modFile (m, Nothing   ) = generatedFile context m
        modFile (m, Just file ) | "//*hs" ?== file = file
                                | otherwise        = modFile (m, Nothing)
    map modFile <$> contextFiles context

generatedFile :: Context -> String -> FilePath
generatedFile context moduleName =
    contextPath context -/- "build" -/- replaceEq '.' '/' moduleName <.> "hs"

contextFiles :: Context -> Action [(String, Maybe FilePath)]
contextFiles context @ Context {..} = do
    let path = contextPath context
    srcDirs <- fmap sort . pkgDataList $ SrcDirs path
    modules <- fmap sort . pkgDataList $ Modules path
    let dirs = [ pkgPath package -/- dir | dir <- srcDirs ]
    zip modules <$> findModuleFiles (path -/- "build/autogen" : dirs) modules

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
moduleFilesOracle = void $ do
    void $ addOracle $ \(ModuleFilesKey (dirs, modules)) -> do
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
            putError $ "Module " ++ m ++ " has more than one source file: "
                ++ f1 ++ " and " ++ f2 ++ "."

        return $ lookupAll modules pairs

    gens <- newCache $ \context -> do
        files <- contextFiles context
        return $ Map.fromList [ (generatedFile context modName, (src, builder))
                              | (modName, Just src) <- files
                              , let Just builder = determineBuilder src ]

    addOracle $ \(Generator (context, file)) -> Map.lookup file <$> gens context
