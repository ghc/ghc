{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Oracles.ModuleFiles (
    decodeModule, encodeModule, findGenerator, haskellSources, moduleFilesOracle
    ) where

import qualified Data.HashMap.Strict as Map

import Base
import Context
import Expression
import Oracles.PackageData
import Settings.Paths

newtype ModuleFilesKey = ModuleFilesKey (Stage, Package)
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

newtype Generator = Generator (Stage, Package, FilePath)
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- The following generators and corresponding source extensions are supported:
determineBuilder :: FilePath -> Maybe Builder
determineBuilder file = case takeExtension file of
    ".x"   -> Just Alex
    ".y"   -> Just Happy
    ".ly"  -> Just Happy
    ".hsc" -> Just Hsc2Hs
    _      -> Nothing

-- | Given a module name extract the directory and file name, e.g.:
--
-- > decodeModule "Data.Functor.Identity" == ("Data/Functor", "Identity")
-- > decodeModule "Prelude"               == ("", "Prelude")
decodeModule :: String -> (FilePath, String)
decodeModule modName = (intercalate "/" (init xs), last xs)
  where
    xs = words $ replaceEq '.' ' ' modName

-- | Given the directory and file name find the corresponding module name, e.g.:
--
-- > encodeModule "Data/Functor" "Identity.hs" == "Data.Functor.Identity"
-- > encodeModule "" "Prelude"                 == "Prelude"
-- > uncurry encodeModule (decodeModule name)  == name
encodeModule :: FilePath -> String -> String
encodeModule dir file
    | dir == "" =                                takeBaseName file
    | otherwise = replaceEq '/' '.' dir ++ '.' : takeBaseName file

-- | Find the generator for a given 'Context' and a source file. For example:
-- findGenerator (Context Stage1 compiler vanilla)
--               "_build/stage1/compiler/build/Lexer.hs"
-- == Just ("compiler/parser/Lexer.x", Alex)
-- findGenerator (Context Stage1 base vanilla)
--               "_build/stage1/base/build/Prelude.hs"
-- == Nothing
findGenerator :: Context -> FilePath -> Action (Maybe (FilePath, Builder))
findGenerator Context {..} file = do
    maybeSource <- askOracle $ Generator (stage, package, file)
    return $ do
        source  <- maybeSource
        builder <- determineBuilder source
        return (source, builder)

-- | Find all Haskell source files for a given 'Context'.
haskellSources :: Context -> Action [FilePath]
haskellSources context = do
    let autogen = buildPath context -/- "autogen"
    -- Generated source files live in buildPath and have extension "hs", except
    -- for GHC/Prim.hs that lives in autogen. TODO: fix the inconsistency?
    let modFile ("GHC.Prim", _) = autogen -/- "GHC/Prim.hs"
        modFile (m, Nothing   ) = generatedFile context m
        modFile (m, Just file )
            | takeExtension file `elem` [".hs", ".lhs"] = file
            | otherwise = generatedFile context m
    map modFile <$> contextFiles context

generatedFile :: Context -> String -> FilePath
generatedFile context moduleName =
    buildPath context -/- replaceEq '.' '/' moduleName <.> "hs"

contextFiles :: Context -> Action [(String, Maybe FilePath)]
contextFiles context@Context {..} = do
    modules <- fmap sort . pkgDataList . Modules $ buildPath context
    zip modules <$> askOracle (ModuleFilesKey (stage, package))

-- | This is an important oracle whose role is to find and cache module source
-- files. It takes a 'Stage' and a 'Package', looks up corresponding source
-- directories @dirs@ and a sorted list of module names @modules@, and for each
-- module, e.g. @A.B.C@, returns a 'FilePath' of the form @dir/A/B/C.extension@,
-- such that @dir@ belongs to @dirs@, and file @dir/A/B/C.extension@ exists, or
-- 'Nothing' if there is no such file. If more than one matching file is found
-- an error is raised. For example, for 'Stage1' and 'compiler', @dirs@ will
-- contain ["compiler/codeGen", "compiler/parser"], and @modules@ will contain
-- ["CodeGen.Platform.ARM", "Config", "Lexer"]; the oracle will produce a list
-- containing [Just "compiler/codeGen/CodeGen/Platform/ARM.hs", Nothing,
-- Just "compiler/parser/Lexer.x"]. The oracle ignores @.(l)hs-boot@ files.
moduleFilesOracle :: Rules ()
moduleFilesOracle = void $ do
    void $ addOracle $ \(ModuleFilesKey (stage, package)) -> do
        let path = buildPath $ vanillaContext stage package
        srcDirs <-             pkgDataList $ SrcDirs path
        modules <- fmap sort . pkgDataList $ Modules path
        let dirs = (path -/- "autogen") : map (pkgPath package -/-) srcDirs
            modDirFiles = groupSort $ map decodeModule modules
        result <- fmap concat . forM dirs $ \dir -> do
            todo <- filterM (doesDirectoryExist . (dir -/-) . fst) modDirFiles
            forM todo $ \(mDir, mFiles) -> do
                let fullDir = unifyPath $ dir -/- mDir
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

    -- Optimisation: we discard .(l)hs files here, because they are never used
    -- as generators, and hence would be discarded in 'findGenerator' anyway.
    generators <- newCache $ \(stage, package) -> do
        let context = vanillaContext stage package
        files <- contextFiles context
        return $ Map.fromList [ (generatedFile context modName, src)
                              | (modName, Just src) <- files
                              , takeExtension src `notElem` [".hs", ".lhs"] ]

    addOracle $ \(Generator (stage, package, file)) ->
        Map.lookup file <$> generators (stage, package)
