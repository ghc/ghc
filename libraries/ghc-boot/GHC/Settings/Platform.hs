-- Note [Settings file]
-- ~~~~~~~~~~~~~~~~~~~~
--
-- GHC has a file, `${top_dir}/settings`, which is the main source of run-time
-- configuration. ghc-pkg needs just a little bit of it: the target platform CPU
-- arch and OS. It uses that to figure out what subdirectory of `~/.ghc` is
-- associated with the current version/target.
--
-- This module has just enough code to read key value pairs from the settings
-- file, and read the target platform from those pairs.
--
-- The  "0" suffix is because the caller will partially apply it, and that will
-- in turn be used a few more times.
module GHC.Settings.Platform where

import Prelude -- See Note [Why do we import Prelude here?]

import GHC.BaseDir
import GHC.Platform
import GHC.Settings.Utils

import Data.Map (Map)
import qualified Data.Map as Map

-----------------------------------------------------------------------------
-- parts of settings file

getTargetPlatform
  :: FilePath -> RawSettings -> Either String Platform
getTargetPlatform settingsFile mySettings = do
  let
    getBooleanSetting = getBooleanSetting0 settingsFile mySettings
    readSetting :: (Show a, Read a) => String -> Either String a
    readSetting = readSetting0 settingsFile mySettings

  targetArch <- readSetting "target arch"
  targetOS <- readSetting "target os"
  targetWordSize <- readSetting "target word size"
  targetWordBigEndian <- getBooleanSetting "target word big endian"
  targetLeadingUnderscore <- getBooleanSetting "Leading underscore"
  targetUnregisterised <- getBooleanSetting "Unregisterised"
  targetHasGnuNonexecStack <- getBooleanSetting "target has GNU nonexec stack"
  targetHasIdentDirective <- getBooleanSetting "target has .ident directive"
  targetHasSubsectionsViaSymbols <- getBooleanSetting "target has subsections via symbols"
  crossCompiling <- getBooleanSetting "cross compiling"

  pure $ Platform
    { platformMini = PlatformMini
      { platformMini_arch = targetArch
      , platformMini_os = targetOS
      }
    , platformWordSize = targetWordSize
    , platformByteOrder = if targetWordBigEndian then BigEndian else LittleEndian
    , platformUnregisterised = targetUnregisterised
    , platformHasGnuNonexecStack = targetHasGnuNonexecStack
    , platformHasIdentDirective = targetHasIdentDirective
    , platformHasSubsectionsViaSymbols = targetHasSubsectionsViaSymbols
    , platformIsCrossCompiling = crossCompiling
    , platformLeadingUnderscore = targetLeadingUnderscore
    }

-----------------------------------------------------------------------------
-- settings file helpers

type RawSettings = Map String String

-- | See Note [Settings file] for "0" suffix
getSetting0
  :: FilePath -> RawSettings -> String -> Either String String
getSetting0 settingsFile mySettings key = case Map.lookup key mySettings of
  Just xs -> Right xs
  Nothing -> Left $ "No entry for " ++ show key ++ " in " ++ show settingsFile

-- | See Note [Settings file] for "0" suffix
getFilePathSetting0
  :: FilePath -> FilePath -> RawSettings -> String -> Either String String
getFilePathSetting0 top_dir settingsFile mySettings key =
  expandTopDir top_dir <$> getSetting0 settingsFile mySettings key

-- | See Note [Settings file] for "0" suffix
getBooleanSetting0
  :: FilePath -> RawSettings -> String -> Either String Bool
getBooleanSetting0 settingsFile mySettings key = do
  rawValue <- getSetting0 settingsFile mySettings key
  case rawValue of
    "YES" -> Right True
    "NO" -> Right False
    xs -> Left $ "Bad value for " ++ show key ++ ": " ++ show xs

-- | See Note [Settings file] for "0" suffix
readSetting0
  :: (Show a, Read a) => FilePath -> RawSettings -> String -> Either String a
readSetting0 settingsFile mySettings key = case Map.lookup key mySettings of
  Just xs -> case maybeRead xs of
    Just v -> Right v
    Nothing -> Left $ "Failed to read " ++ show key ++ " value " ++ show xs
  Nothing -> Left $ "No entry for " ++ show key ++ " in " ++ show settingsFile
