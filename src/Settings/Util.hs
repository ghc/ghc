module Settings.Util (
    -- Primitive settings elements
    arg, argM,
    argSetting, argSettingList,
    getFlag, getSetting, getSettingList,
    getPkgData, getPkgDataList,
    appendCcArgs,
    needBuilder
    -- argBuilderPath, argStagedBuilderPath,
    -- argPackageKey, argPackageDeps, argPackageDepKeys, argSrcDirs,
    -- argIncludeDirs, argDepIncludeDirs,
    -- argConcat, argConcatPath, argConcatSpace,
    -- argPairs, argPrefix, argPrefixPath,
    -- argPackageConstraints,
    ) where

import Builder
import Expression
import Oracles.Base
import Oracles.Flag
import Oracles.Setting
import Oracles.PackageData
import Settings.User
import Settings.TargetDirectory

-- A single argument.
arg :: String -> Args
arg = append . return

argM :: Action String -> Args
argM = appendM . fmap return

argSetting :: Setting -> Args
argSetting = argM . setting

argSettingList :: SettingList -> Args
argSettingList = appendM . settingList

getFlag :: Flag -> Expr Bool
getFlag = lift . flag

getSetting :: Setting -> Expr String
getSetting = lift . setting

getSettingList :: SettingList -> Expr [String]
getSettingList = lift . settingList

getPkgData :: (FilePath -> PackageData) -> Expr String
getPkgData key = do
    stage <- getStage
    pkg   <- getPackage
    lift . pkgData . key $ targetPath stage pkg

getPkgDataList :: (FilePath -> PackageDataList) -> Expr [String]
getPkgDataList key = do
    stage <- getStage
    pkg   <- getPackage
    lift . pkgDataList . key $ targetPath stage pkg

-- Pass arguments to Gcc and corresponding lists of sub-arguments of GhcCabal
appendCcArgs :: [String] -> Args
appendCcArgs xs = do
    stage <- getStage
    mconcat [ builder (Gcc stage)  ? append xs
            , builder (GccM stage) ? append xs
            , builder GhcCabal     ? appendSub "--configure-option=CFLAGS" xs
            , builder GhcCabal     ? appendSub "--gcc-options" xs ]

-- Make sure a builder exists on the given path and rebuild it if out of date.
-- If laxDependencies is true (Settings/User.hs) then we do not rebuild GHC
-- even if it is out of date (can save a lot of build time when changing GHC).
needBuilder :: Builder -> Action ()
needBuilder ghc @ (Ghc stage) = do
    path <- builderPath ghc
    if laxDependencies then orderOnly [path] else need [path]

needBuilder builder = do
    path <- builderPath builder
    need [path]

-- TODO: do '-ticky' in all debug ways?
-- wayHcArgs :: Way -> Args
-- wayHcArgs (Way _ units) = args
--     [ if (Dynamic    `elem` units)
--       then args ["-fPIC", "-dynamic"]
--       else arg "-static"
--     , when (Threaded   `elem` units) $ arg "-optc-DTHREADED_RTS"
--     , when (Debug      `elem` units) $ arg "-optc-DDEBUG"
--     , when (Profiling  `elem` units) $ arg "-prof"
--     , when (Logging    `elem` units) $ arg "-eventlog"
--     , when (Parallel   `elem` units) $ arg "-parallel"
--     , when (GranSim    `elem` units) $ arg "-gransim"
--     , when (units == [Debug] || units == [Debug, Dynamic]) $
--       args ["-ticky", "-DTICKY_TICKY"] ]
