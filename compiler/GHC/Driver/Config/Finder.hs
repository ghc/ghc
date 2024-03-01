module GHC.Driver.Config.Finder (
    FinderOpts(..),
    initFinderOpts
  ) where

import GHC.Prelude

import qualified GHC.Data.Strict as Strict
import GHC.Driver.DynFlags
import GHC.Unit.Finder.Types
import GHC.Data.FastString
-- TODO: this must not be here, replace with 'GHC.Data.OsPath'
import GHC.Unit.Module.Location

-- | Create a new 'FinderOpts' from DynFlags.
initFinderOpts :: DynFlags -> FinderOpts
initFinderOpts flags = FinderOpts
  { finder_importPaths = fmap unsafeEncodeUtf $ importPaths flags
  , finder_lookupHomeInterfaces = isOneShot (ghcMode flags)
  , finder_bypassHiFileCheck = MkDepend == (ghcMode flags)
  , finder_ways = ways flags
  , finder_enableSuggestions = gopt Opt_HelpfulErrors flags
  , finder_workingDirectory = fmap unsafeEncodeUtf $ Strict.fromLazy $ workingDirectory flags
  , finder_thisPackageName  = mkFastString <$> thisPackageName flags
  , finder_hiddenModules = hiddenModules flags
  , finder_reexportedModules = reexportedModules flags
  , finder_hieDir = fmap unsafeEncodeUtf $ Strict.fromLazy $ hieDir flags
  , finder_hieSuf = unsafeEncodeUtf $ hieSuf flags
  , finder_hiDir = fmap unsafeEncodeUtf $ Strict.fromLazy $ hiDir flags
  , finder_hiSuf = unsafeEncodeUtf $ hiSuf_ flags
  , finder_dynHiSuf = unsafeEncodeUtf $ dynHiSuf_ flags
  , finder_objectDir = fmap unsafeEncodeUtf $ Strict.fromLazy $ objectDir flags
  , finder_objectSuf = unsafeEncodeUtf $ objectSuf_ flags
  , finder_dynObjectSuf = unsafeEncodeUtf $ dynObjectSuf_ flags
  , finder_stubDir = fmap unsafeEncodeUtf $ Strict.fromLazy $ stubDir flags
  }
