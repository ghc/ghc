module GHC.Driver.Config.Finder (
    FinderOpts(..),
    initFinderOpts
  ) where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Unit.Finder

-- | Create a new 'FinderOpts' from DynFlags.
initFinderOpts :: DynFlags -> FinderOpts
initFinderOpts flags = FinderOpts
  { finder_importPaths = importPaths flags
  , finder_lookupHomeInterfaces = isOneShot (ghcMode flags)
  , finder_bypassHiFileCheck = MkDepend == (ghcMode flags)
  , finder_ways = ways flags
  , finder_enableSuggestions = gopt Opt_HelpfulErrors flags
  , finder_hieDir = hieDir flags
  , finder_hieSuf = hieSuf flags
  , finder_hiDir = hiDir flags
  , finder_hiSuf = hiSuf flags
  , finder_objectDir = objectDir flags
  , finder_objectSuf = objectSuf flags
  , finder_stubDir = stubDir flags
  }
