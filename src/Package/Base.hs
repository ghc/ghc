{-# LANGUAGE NoImplicitPrelude #-}
module Package.Base (
    module Base,
    module Ways,
    module Util,
    module Oracles,
    Package (..), Settings (..), TodoItem (..),
    defaultSettings, libraryPackage,
    commonCcArgs, commonLdArgs, commonCppArgs, commonCcWarninigArgs,
    pathArgs, packageArgs, includeArgs, pkgHsSources,
    pkgDepObjects, pkgLibObjects,
    argSizeLimit,
    sourceDependecies,
    argList, argListWithComment,
    argListPath
    ) where

import Base
import Ways
import Util
import Oracles

data Settings = Settings
     {
         customConfArgs  :: Args,
         customCcArgs    :: Args,
         customLdArgs    :: Args,
         customCppArgs   :: Args,
         customDllArgs   :: Args,
         registerPackage :: Bool,
         ways            :: Action [Way]
     }

defaultSettings :: Stage -> Settings
defaultSettings stage =
    Settings mempty mempty mempty mempty mempty True (defaultWays stage)

-- Stage is the stage of the GHC that we use to build the package
-- FilePath is the directory to put the build results
-- Settings may be different for different combinations of Stage & FilePath
type TodoItem = (Stage, FilePath, Settings)

-- pkgPath is the path to the source code relative to the root
data Package = Package
     {
         pkgName :: String,    -- For example: "deepseq"
         pkgPath :: FilePath,  -- "libraries/deepseq"
         pkgTodo :: [TodoItem] -- [(Stage1, "dist-install", defaultSettings)]
     }

libraryPackage :: String -> [Stage] -> (Stage -> Settings) -> Package
libraryPackage name stages settings =
    Package
        name
        (toStandard $ "libraries" </> name)
        [ (stage
        , if stage == Stage0 then "dist-boot" else "dist-install"
        , settings stage)
        | stage <- stages ]

commonCcArgs :: Args
commonCcArgs = when Validating $ arg ["-Werror", "-Wall"]

commonLdArgs :: Args
commonLdArgs = mempty -- TODO: Why empty? Perhaps drop it altogether?

commonCppArgs :: Args
commonCppArgs = mempty -- TODO: Why empty? Perhaps drop it altogether?

commonCcWarninigArgs :: Args
commonCcWarninigArgs = when Validating $ arg
       [ when GccIsClang                      $ arg "-Wno-unknown-pragmas"
       , when (not GccIsClang && not GccLt46) $ arg "-Wno-error=inline"
       , when (GccIsClang && not GccLt46 && windowsHost) $
         arg "-Werror=unused-but-set-variable" ]

pathArgs :: ShowArgs a => String -> FilePath -> a -> Args
pathArgs key path as =
    map (\a -> key ++ toStandard (normaliseEx $ path </> a)) <$> arg as

packageArgs :: Stage -> FilePath -> Args
packageArgs stage pkgData = do
    usePackageKey <- SupportsPackageKey || stage /= Stage0
    arg [ arg "-hide-all-packages"
        , arg "-no-user-package-db"
        , arg "-include-pkg-deps"
        , when (stage == Stage0) $
          arg "-package-db libraries/bootstrapping.conf"
        , keyArgs usePackageKey ]
  where
    keyArgs True  = productArgs "-this-package-key" (PackageKey pkgData) <>
                    productArgs "-package-key"      (DepKeys    pkgData)
    keyArgs False = productArgs "-package-name"     (PackageKey pkgData) <>
                    productArgs "-package"          (Deps       pkgData)

includeArgs :: FilePath -> FilePath -> Args
includeArgs path dist =
    let pkgData  = toStandard $ path </> dist </> "package-data.mk"
        buildDir = toStandard $ path </> dist </> "build"
    in arg [ arg "-i"
           , pathArgs "-i" path $ SrcDirs pkgData
           , concatArgs ["-i", "-I"]
             [buildDir, toStandard $ buildDir </> "autogen"]
           , pathArgs "-I" path $ IncludeDirs pkgData
           , arg "-optP-include" -- TODO: Shall we also add -cpp?
           , concatArgs "-optP" $
             toStandard $ buildDir </> "autogen/cabal_macros.h" ]

pkgHsSources :: FilePath -> FilePath -> Action [FilePath]
pkgHsSources path dist = do
    let pkgData = path </> dist </> "package-data.mk"
    dirs <- map (path </>) <$> arg (SrcDirs pkgData)
    findModuleFiles pkgData dirs [".hs", ".lhs"]

-- Find objects we depend on (we don't want to depend on split objects)
-- TODO: look for non-hs objects too
pkgDepObjects :: FilePath -> FilePath -> Way -> Action [FilePath]
pkgDepObjects path dist way = do
    let pkgData  = path </> dist </> "package-data.mk"
        buildDir = path </> dist </> "build"
    dirs <- map (normaliseEx . (path </>)) <$> arg (SrcDirs pkgData)
    fmap concat $ forM dirs $ \d ->
        map (toStandard . (buildDir ++) . (-<.> osuf way) . drop (length d))
        <$> (findModuleFiles pkgData [d] [".hs", ".lhs"])

-- Find objects that go to library
pkgLibObjects :: FilePath -> FilePath -> Stage -> Way -> Action [FilePath]
pkgLibObjects path dist stage way = do
    let pkgData  = path </> dist </> "package-data.mk"
        buildDir = path </> dist </> "build"
    split <- splitObjects stage
    if split
    then do
         let suffixes = ["_" ++ osuf way ++ "_split//*"]
         findModuleFiles pkgData [buildDir] suffixes
    else pkgDepObjects path dist way

findModuleFiles :: FilePath -> [FilePath] -> [String] -> Action [FilePath]
findModuleFiles pkgData directories suffixes = do
    modPaths <- map (replaceEq '.' pathSeparator) <$> arg (Modules pkgData)
    fileList <- forM directories $ \dir     ->
                forM modPaths    $ \modPath ->
                forM suffixes    $ \suffix  -> do
                    let file = dir </> modPath ++ suffix
                    when (doesDirectoryExist $ dropFileName file) $ return file
    files <- getDirectoryFiles "" $ concat $ concat fileList
    return $ map (toStandard . normaliseEx) files

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
argListPath dir (Package name _ _) stage =
    dir </> takeBaseName name ++ " (stage " ++ show stage ++ ")" <.> "txt"
