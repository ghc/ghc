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

