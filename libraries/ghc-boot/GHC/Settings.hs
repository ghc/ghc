module GHC.Settings where

import Prelude -- See note [Why do we import Prelude here?]

import GHC.BaseDir
import GHC.Platform

import Data.Char (isSpace)
import Data.Map (Map)
import qualified Data.Map as Map

-----------------------------------------------------------------------------
-- parts of settings file

getTargetPlatform settingsFile mySettings key = do
  let
    getSetting = getSetting0 settingsFile mySettings
    getBooleanSetting = getBooleanSetting0 settingsFile mySettings
    readSetting = readSetting0 settingsFile mySettings

  targetPlatformString <- getSetting "target platform string"
  targetArch <- readSetting "target arch"
  targetOS <- readSetting "target os"
  targetWordSize <- readSetting "target word size"
  targetUnregisterised <- getBooleanSetting "Unregisterised"
  targetHasGnuNonexecStack <- readSetting "target has GNU nonexec stack"
  targetHasIdentDirective <- readSetting "target has .ident directive"
  targetHasSubsectionsViaSymbols <- readSetting "target has subsections via symbols"
  crossCompiling <- getBooleanSetting "cross compiling"

  pure $ Platform
    { platformArch = targetArch
    , platformOS   = targetOS
    , platformWordSize = targetWordSize
    , platformUnregisterised = targetUnregisterised
    , platformHasGnuNonexecStack = targetHasGnuNonexecStack
    , platformHasIdentDirective = targetHasIdentDirective
    , platformHasSubsectionsViaSymbols = targetHasSubsectionsViaSymbols
    , platformIsCrossCompiling = crossCompiling
    }

-----------------------------------------------------------------------------
-- settings file helpers

type Settings = Map String String

getSetting0
  :: FilePath -> FilePath -> Settings -> String -> Either String String
getSetting0 top_settingsFile mySettings key = case Map.lookup key mySettings of
  Just xs -> Right xs
  Nothing -> Left $ "No entry for " ++ show key ++ " in " ++ show settingsFile

getFilePathSetting0
  :: FilePath -> FilePath -> Settings -> String -> Either String String
getFilePathSetting0 top_dir settingsFile mySettings key =
  expandTopDir top_dir <$> getSetting0 settingsFile mySettings key

getBooleanSetting0
  :: FilePath -> Settings -> String -> Either String Bool
getBooleanSetting0 settingsFile mySettings key = do
  rawValue <- getSetting0 settingsFile mySettings key
  case rawValue of
    "YES" -> Right True
    "NO" -> Right False
    xs -> Left $ "Bad value for " ++ show key ++ ": " ++ show xs

readSetting0
  :: (Show a, Read a) => FilePath -> Settings -> String -> Either String a
readSetting0 settingsFile mySettings key = case Map.lookup key mySettings of
  Just xs -> case maybeRead xs of
    Just v -> Right v
    Nothing -> Left $ "Failed to read " ++ show key ++ " value " ++ show xs
  Nothing -> Left $ "No entry for " ++ show key ++ " in " ++ show settingsFile

-----------------------------------------------------------------------------
-- read helpers

maybeRead :: Read a => String -> Maybe a
maybeRead str = case reads str of
  [(x, "")] -> Just x
  _ -> Nothing

maybeReadFuzzy :: Read a => String -> Maybe a
maybeReadFuzzy str = case reads str of
  [(x, s)] | all isSpace s -> Just x
  _ -> Nothing
