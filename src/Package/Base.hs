{-# LANGUAGE NoImplicitPrelude #-}
module Package.Base (
    module Base,
    module Ways,
    module Util,
    module Oracles,
    -- Package (..), Settings (..), TodoItem (..),
    -- defaultSettings, library, customise, updateSettings,
    -- commonCcArgs, commonLdArgs, commonCppArgs, commonCcWarninigArgs,
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
import Settings
import qualified System.Directory as S

--pathArgs :: ShowArgs a => String -> FilePath -> a -> Args
--pathArgs key path as = map (\a -> key ++ unifyPath (path </> a)) <$> args as

prefixedPath :: String -> [Settings] -> Settings
prefixedPath prefix = argPrefix prefix . argConcatPath . sconcat

--includeGccArgs :: FilePath -> FilePath -> Args
--includeGccArgs path dist =
--    let pathDist = path </> dist
--        autogen  = pathDist </> "build/autogen"
--    in args [ arg $ "-I" ++ unifyPath autogen
--            , pathArgs "-I" path $ IncludeDirs pathDist
--            , pathArgs "-I" path $ DepIncludeDirs pathDist ]


includeGccSettings :: Settings
includeGccSettings = mconcat
    [ prefixedPath "-I" [argBuildPath, argBuildDir, arg "build", arg "autogen"]
    , argPrefix "-I" $ argPaths ...
    , prefixedPath "-I" [argBuildPath, argIncludeDirs ] -- wrong
    , prefixedPath "-I" [argBuildPath, argDepIncludeDirs ]]

includeGhcSettings :: Settings
includeGhcSettings =
    let buildDir = argBuildPath `fence` argSrcDirs
    in arg "-i" `fence`
       mconcat
       [ argPathList "-i" [argBuildPath, argSrcDirs]
       , argPath "-i" buildDir
       , argPath "-I" buildDir
       , argPathList "-i" [buildDir, arg "autogen"]
       , argPathList "-I" [buildDir, arg "autogen"]
       , argPathList "-I" [argBuildPath, argIncludeDirs]
       , arg "-optP-include" -- TODO: Shall we also add -cpp?
       , argPathList "-optP" [buildDir, arg "autogen/cabal_macros.h"] ]


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
