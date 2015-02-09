{-# LANGUAGE NoImplicitPrelude #-}
module Package.Base (
    module Base,
    module Ways,
    module Util,
    module Oracles,
    Package (..), Settings (..), TodoItem (..),
    defaultSettings, library, customise, updateSettings,
    commonCcArgs, commonLdArgs, commonCppArgs, commonCcWarninigArgs,
    pathArgs, packageArgs,
    includeGccArgs, includeGhcArgs, pkgHsSources,
    pkgDepHsObjects, pkgLibHsObjects, pkgCObjects,
    argSizeLimit,
    sourceDependecies,
    argList, argListWithComment,
    argListPath
    ) where

import Base
import Ways
import Util
import Oracles
import qualified System.Directory as S

data Settings = Settings
     {
         customConfArgs  :: Args,         -- custom args for configure
         customCcArgs    :: Args,         -- custom args for Gcc
         customLdArgs    :: Args,         -- custom args for Ld
         customCppArgs   :: Args,         -- custom args for C preprocessor
         customDllArgs   :: Args,         -- custom dll args
         registerPackage :: Bool,         -- do we need to call ghc-pkg update?
         ways            :: Action [Way], -- ways to build
         buildWhen       :: Condition     -- skip the package if need be, e.g.
     }                                    -- don't build unix on Windows

defaultSettings :: Stage -> Settings
defaultSettings stage = Settings
                        {
                            customConfArgs  = mempty,
                            customCcArgs    = mempty,
                            customLdArgs    = mempty, -- currently not used
                            customCppArgs   = mempty, -- currently not used
                            customDllArgs   = mempty, -- only for compiler
                            registerPackage = True,
                            ways            = defaultWays stage,
                            buildWhen       = return True
                        }

-- Stage is the stage of the GHC that we use to build the package
-- FilePath is the directory to put the build results (relative to pkgPath)
-- The typical structure of that directory is:
-- * build/           : contains compiled object code
-- * doc/             : produced by haddock
-- * package-data.mk  : contains output of ghc-cabal applied to pkgCabal.cabal
-- Settings may be different for different combinations of Stage & FilePath
-- TODO: the above may be incorrect, settings seem to *only* depend on the
-- stage. In fact Stage seem to define FilePath and Settings, therefore we
-- can drop the TodoItem and replace it by [Stage] and two functions
--    * distDirectory :: Package -> Stage -> FilePath
--    * settings      :: Package -> Stage -> Settings
type TodoItem = (Stage, FilePath, Settings)

-- pkgPath is the path to the source code relative to the root
data Package = Package
     {
         pkgName  :: String,    -- For example: "deepseq"
         pkgPath  :: FilePath,  -- "libraries/deepseq"
         pkgCabal :: FilePath,  -- "deepseq"
         pkgTodo  :: [TodoItem] -- [(Stage1, "dist-install", defaultSettings)]
     }

updateSettings :: (Settings -> Settings) -> Package -> Package
updateSettings update (Package name path cabal todo) =
    Package name path cabal (map updateTodo todo)
  where
    updateTodo (stage, filePath, settings) = (stage, filePath, update settings)

customise :: Package -> (Package -> Package) -> Package
customise = flip ($)

libraryPackage :: String -> String -> [Stage] -> (Stage -> Settings) -> Package
libraryPackage name cabalName stages settings =
    Package
        name
        (unifyPath $ "libraries" </> name)
        cabalName
        [ (stage
        , if stage == Stage0 then "dist-boot" else "dist-install"
        , settings stage)
        | stage <- stages ]

library :: String -> [Stage] -> Package
library name stages = libraryPackage name name stages defaultSettings

commonCcArgs :: Args
commonCcArgs = when Validating $ args ["-Werror", "-Wall"]

commonLdArgs :: Args
commonLdArgs = mempty -- TODO: Why empty? Perhaps drop it altogether?

commonCppArgs :: Args
commonCppArgs = mempty -- TODO: Why empty? Perhaps drop it altogether?

commonCcWarninigArgs :: Args
commonCcWarninigArgs = when Validating $
    args [ when GccIsClang                      $ arg "-Wno-unknown-pragmas"
         , when (not GccIsClang && not GccLt46) $ arg "-Wno-error=inline"
         , when (GccIsClang && not GccLt46 && windowsHost) $
           arg "-Werror=unused-but-set-variable" ]

pathArgs :: ShowArgs a => String -> FilePath -> a -> Args
pathArgs key path as = map (\a -> key ++ unifyPath (path </> a)) <$> args as

packageArgs :: Stage -> FilePath -> Args
packageArgs stage pathDist = do
    usePackageKey <- SupportsPackageKey || stage /= Stage0
    args [ arg "-hide-all-packages"
         , arg "-no-user-package-db"
         , arg "-include-pkg-deps"
         , when (stage == Stage0) $
           arg "-package-db libraries/bootstrapping.conf"
         , if usePackageKey
           then productArgs ["-this-package-key"] [arg  $ PackageKey pathDist]
             <> productArgs ["-package-key"     ] [args $ DepKeys    pathDist]
           else productArgs ["-package-name"    ] [arg  $ PackageKey pathDist]
             <> productArgs ["-package"         ] [args $ Deps       pathDist]
         ]

includeGccArgs :: FilePath -> FilePath -> Args
includeGccArgs path dist =
    let pathDist = path </> dist
        autogen  = pathDist </> "build/autogen"
    in args [ arg $ "-I" ++ unifyPath autogen
            , pathArgs "-I" path $ IncludeDirs pathDist
            , pathArgs "-I" path $ DepIncludeDirs pathDist ]

includeGhcArgs :: FilePath -> FilePath -> Args
includeGhcArgs path dist =
    let pathDist = path </> dist
        buildDir = unifyPath $ pathDist </> "build"
    in args [ arg "-i"
            , pathArgs "-i" path $ SrcDirs pathDist
            , concatArgs ["-i", "-I"]
              [buildDir, unifyPath $ buildDir </> "autogen"]
            , pathArgs "-I" path $ IncludeDirs pathDist
            , arg "-optP-include" -- TODO: Shall we also add -cpp?
            , concatArgs ["-optP"]
              [unifyPath $ buildDir </> "autogen/cabal_macros.h"]
            ]

pkgHsSources :: FilePath -> FilePath -> Action [FilePath]
pkgHsSources path dist = do
    let pathDist = path </> dist
        autogen = pathDist </> "build/autogen"
    dirs <- map (path </>) <$> args (SrcDirs pathDist)
    findModuleFiles pathDist (autogen:dirs) [".hs", ".lhs"]

-- TODO: look for non-{hs,c} objects too

-- Find Haskell objects we depend on (we don't want to depend on split objects)
pkgDepHsObjects :: FilePath -> FilePath -> Way -> Action [FilePath]
pkgDepHsObjects path dist way = do
    let pathDist = path </> dist
        buildDir = pathDist </> "build"
    dirs <- map (dropWhileEnd isPathSeparator . unifyPath . (path </>))
            <$> args (SrcDirs pathDist)
    fmap concat $ forM dirs $ \d ->
        map (unifyPath . (buildDir ++) . (-<.> osuf way) . drop (length d))
        <$> (findModuleFiles pathDist [d] [".hs", ".lhs"])

pkgCObjects :: FilePath -> FilePath -> Way -> Action [FilePath]
pkgCObjects path dist way = do
    let pathDist = path </> dist
        buildDir = pathDist </> "build"
    srcs <- args $ CSrcs pathDist
    return $ map (unifyPath . (buildDir </>) . (-<.> osuf way)) srcs

-- Find Haskell objects that go to library
pkgLibHsObjects :: FilePath -> FilePath -> Stage -> Way -> Action [FilePath]
pkgLibHsObjects path dist stage way = do
    let pathDist = path </> dist
        buildDir = unifyPath $ pathDist </> "build"
    split <- splitObjects stage
    depObjs <- pkgDepHsObjects path dist way
    if split
    then do
         need depObjs -- Otherwise, split objects may not yet be available
         let suffix = "_" ++ osuf way ++ "_split/*." ++ osuf way
         findModuleFiles pathDist [buildDir] [suffix]
    else do return depObjs

findModuleFiles :: FilePath -> [FilePath] -> [String] -> Action [FilePath]
findModuleFiles pathDist directories suffixes = do
    modPaths <- map (replaceEq '.' pathSeparator) <$> args (Modules pathDist)
    fileList <- forM [ dir </> modPath ++ suffix
                     | dir     <- directories
                     , modPath <- modPaths
                     , suffix  <- suffixes
                     ] $ \file -> do
                         let dir = takeDirectory file
                         dirExists <- liftIO $ S.doesDirectoryExist dir
                         when dirExists $ return $ unifyPath file
    files <- getDirectoryFiles "" fileList
    return $ map unifyPath files

-- The argument list has a limited size on Windows. Since Windows 7 the limit
-- is 32768 (theoretically). In practice we use 31000 to leave some breathing
-- space for the builder's path & name, auxiliary flags, and other overheads.
-- Use this function to set limits for other operating systems if necessary.
argSizeLimit :: Action Int
argSizeLimit = do
    windows <- windowsHost
    return $ if windows
             then 31000
             else 4194304 -- Cabal needs a bit more than 2MB!

-- List of source files, which need to be tracked by the build system
-- to make sure the argument lists have not changed.
sourceDependecies :: [FilePath]
sourceDependecies = [ "shake/src/Package/Base.hs"
                    , "shake/src/Oracles/Base.hs"
                    , "shake/src/Oracles/Flag.hs"
                    , "shake/src/Oracles/Option.hs"
                    , "shake/src/Oracles/Builder.hs"
                    , "shake/src/Oracles/PackageData.hs"
                    , "shake/src/Ways.hs"
                    , "shake/src/Util.hs"
                    , "shake/src/Oracles.hs" ]

-- Convert Builder's argument list to a printable String
argListWithComment :: String -> Builder -> Args -> Action String
argListWithComment comment builder args = do
    args' <- args
    return $ show builder ++ " arguments"
           ++ (if null comment then "" else " (" ++ comment ++ ")")
           ++ ":\n" ++ concatMap (\s -> "    " ++ s ++ "\n") args'

argList :: Builder -> Args -> Action String
argList = argListWithComment ""

-- Path to argument list for a given Package/Stage combination
argListPath :: FilePath -> Package -> Stage -> FilePath
argListPath dir (Package name _ _ _) stage =
    dir </> takeBaseName name ++ " (stage " ++ show stage ++ ")" <.> "txt"
