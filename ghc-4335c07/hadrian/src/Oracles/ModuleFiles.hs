{-# LANGUAGE TypeFamilies #-}
module Oracles.ModuleFiles (
    decodeModule, encodeModule, findGenerator, hsSources, hsObjects, moduleFilesOracle
    ) where

import qualified Data.HashMap.Strict as Map

import Base
import Builder
import Context
import GHC
import Oracles.PackageData

newtype ModuleFiles = ModuleFiles (Stage, Package)
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult ModuleFiles = [Maybe FilePath]

newtype Generator = Generator (Stage, Package, FilePath)
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult Generator = Maybe FilePath

-- | We scan for the following Haskell source extensions when looking for module
-- files. Note, we do not list "*.(l)hs-boot" files here, as they can never
-- appear by themselves and always have accompanying "*.(l)hs" master files.
haskellExtensions :: [String]
haskellExtensions = [".hs", ".lhs"]

-- | Non-Haskell source extensions and corresponding builders.
otherExtensions :: [(String, Builder)]
otherExtensions = [ (".x"  , Alex  )
                  , (".y"  , Happy )
                  , (".ly" , Happy )
                  , (".hsc", Hsc2Hs) ]

-- | We match the following file patterns when looking for module files.
moduleFilePatterns :: [FilePattern]
moduleFilePatterns = map ("*" ++) $ haskellExtensions ++ map fst otherExtensions

-- | Given a FilePath determine the corresponding builder.
determineBuilder :: FilePath -> Maybe Builder
determineBuilder file = lookup (takeExtension file) otherExtensions

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
hsSources :: Context -> Action [FilePath]
hsSources context = do
    let modFile (m, Nothing   ) = generatedFile context m
        modFile (m, Just file )
            | takeExtension file `elem` haskellExtensions = return file
            | otherwise = generatedFile context m
    mapM modFile =<< contextFiles context

-- | Find all Haskell object files for a given 'Context'. Note: this is a much
-- simpler function compared to 'hsSources', because all object files live in
-- the build directory regardless of whether they are generated or not.
hsObjects :: Context -> Action [FilePath]
hsObjects context = do
    path    <- buildPath context
    modules <- pkgDataList (Modules path)
    -- GHC.Prim module is only for documentation, we do not actually build it.
    mapM (objectPath context . moduleSource) (filter (/= "GHC.Prim") modules)

-- | Generated module files live in the 'Context' specific build directory.
generatedFile :: Context -> String -> Action FilePath
generatedFile context moduleName = do
    path <- buildPath context
    return $ path -/- moduleSource moduleName

moduleSource :: String -> FilePath
moduleSource moduleName = replaceEq '.' '/' moduleName <.> "hs"

-- | Module files for a given 'Context'.
contextFiles :: Context -> Action [(String, Maybe FilePath)]
contextFiles context@Context {..} = do
    path    <- buildPath context
    modules <- fmap sort . pkgDataList $ Modules path
    zip modules <$> askOracle (ModuleFiles (stage, package))

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
    void . addOracle $ \(ModuleFiles (stage, package)) -> do
        let context = vanillaContext stage package
        path    <- buildPath context
        srcDirs <-             pkgDataList $ SrcDirs path
        modules <- fmap sort . pkgDataList $ Modules path
        autogen <- autogenPath context
        let dirs = autogen : map (pkgPath package -/-) srcDirs
            modDirFiles = groupSort $ map decodeModule modules
        result <- concatForM dirs $ \dir -> do
            todo <- filterM (doesDirectoryExist . (dir -/-) . fst) modDirFiles
            forM todo $ \(mDir, mFiles) -> do
                let fullDir = unifyPath $ dir -/- mDir
                files <- getDirectoryFiles fullDir moduleFilePatterns
                let cmp f = compare (dropExtension f)
                    found = intersectOrd cmp files mFiles
                return (map (fullDir -/-) found, mDir)
        let pairs = sort [ (encodeModule d f, f) | (fs, d) <- result, f <- fs ]
            multi = [ (m, f1, f2) | (m, f1):(n, f2):_ <- tails pairs, m == n ]
        unless (null multi) $ do
            let (m, f1, f2) = head multi
            error $ "Module " ++ m ++ " has more than one source file: "
                ++ f1 ++ " and " ++ f2 ++ "."
        return $ lookupAll modules pairs

    -- Optimisation: we discard Haskell files here, because they are never used
    -- as generators, and hence would be discarded in 'findGenerator' anyway.
    generators <- newCache $ \(stage, package) -> do
        let context = vanillaContext stage package
        files <- contextFiles context
        list  <- sequence [ (,src) <$> generatedFile context modName
                          | (modName, Just src) <- files
                          , takeExtension src `notElem` haskellExtensions ]
        return $ Map.fromList list

    addOracle $ \(Generator (stage, package, file)) ->
        Map.lookup file <$> generators (stage, package)
