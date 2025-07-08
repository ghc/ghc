module GHC.Settings.Utils where

import Prelude -- See Note [Why do we import Prelude here?]

import Data.Char (isSpace)
import Data.Map (Map)
import qualified Data.Map as Map

import GHC.BaseDir
import GHC.Platform.ArchOS
import System.FilePath

import GHC.Toolchain.Target

maybeRead :: Read a => String -> Maybe a
maybeRead str = case reads str of
  [(x, "")] -> Just x
  _ -> Nothing

maybeReadFuzzy :: Read a => String -> Maybe a
maybeReadFuzzy str = case reads str of
  [(x, s)] | all isSpace s -> Just x
  _ -> Nothing


-- Note [Settings file]
-- ~~~~~~~~~~~~~~~~~~~~
--
-- GHC has a file, `${top_dir}/settings`, which is the main source of run-time
-- configuration. ghc-pkg needs just a little bit of it: the target platform CPU
-- arch and OS. It uses that to figure out what subdirectory of `~/.ghc` is
-- associated with the current version/target platform.
--
-- This module has just enough code to read key value pairs from the settings
-- file, and read the target platform from those pairs.

type RawSettings = Map String String

-- | Read target Arch/OS from the settings
getTargetArchOS
  :: Target -- ^ The 'Target' from which to read the 'ArchOS'
  -> ArchOS
getTargetArchOS target = tgtArchOs target

getGlobalPackageDb :: FilePath -> RawSettings -> Either String FilePath
getGlobalPackageDb settingsFile settings = do
  rel_db <- getRawSetting settingsFile settings "Relative Global Package DB"
  return (dropFileName settingsFile </> rel_db)

--------------------------------------------------------------------------------
-- lib/settings

getRawSetting
  :: FilePath -> RawSettings -> String -> Either String String
getRawSetting settingsFile settings key = case Map.lookup key settings of
  Just xs -> Right xs
  Nothing -> Left $ "No entry for " ++ show key ++ " in " ++ show settingsFile

getRawFilePathSetting
  :: FilePath -> FilePath -> RawSettings -> String -> Either String String
getRawFilePathSetting top_dir settingsFile settings key =
  expandTopDir top_dir <$> getRawSetting settingsFile settings key

getRawBooleanSetting
  :: FilePath -> RawSettings -> String -> Either String Bool
getRawBooleanSetting settingsFile settings key = do
  rawValue <- getRawSetting settingsFile settings key
  case rawValue of
    "YES" -> Right True
    "NO" -> Right False
    xs -> Left $ "Bad value for " ++ show key ++ ": " ++ show xs

