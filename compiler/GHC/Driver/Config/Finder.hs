module GHC.Driver.Config.Finder (
    FinderOpts(..),
    initFinderOpts
  ) where

import GHC.Prelude

import GHC.Driver.DynFlags
import GHC.Unit.Finder.Types
import GHC.Data.FastString
import GHC.Data.OsPath
import qualified Data.Map as Map

-- | Create a new 'FinderOpts' from DynFlags.
initFinderOpts :: DynFlags -> FinderOpts
initFinderOpts flags = FinderOpts
  { finder_importPaths = fmap unsafeEncodeUtf $ importPaths flags
  , finder_lookupHomeInterfaces = isOneShot (ghcMode flags)
  , finder_bypassHiFileCheck = MkDepend == (ghcMode flags)
  , finder_ways = ways flags
  , finder_enableSuggestions = gopt Opt_HelpfulErrors flags
  , finder_workingDirectory = fmap unsafeEncodeUtf $ workingDirectory flags
  , finder_thisPackageName  = mkFastString <$> thisPackageName flags
  , finder_hiddenModules = hiddenModules flags
  , finder_reexportedModules = Map.fromList [(known_as, is_as) | ReexportedModule is_as known_as <- reverse (reexportedModules flags)]
  , finder_hieDir = fmap unsafeEncodeUtf $ hieDir flags
  , finder_hieSuf = unsafeEncodeUtf $ hieSuf flags
  , finder_hiDir = fmap unsafeEncodeUtf $ hiDir flags
  , finder_hiSuf = unsafeEncodeUtf $ hiSuf_ flags
  , finder_dynHiSuf = unsafeEncodeUtf $ dynHiSuf_ flags
  , finder_objectDir = fmap unsafeEncodeUtf $ objectDir flags
  , finder_objectSuf = unsafeEncodeUtf $ objectSuf_ flags
  , finder_dynObjectSuf = unsafeEncodeUtf $ dynObjectSuf_ flags
  , finder_stubDir = fmap unsafeEncodeUtf $ stubDir flags
  }
