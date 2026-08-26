module GHC.Unit.Finder.Types
   ( FinderCache (..)
   , FindResult (..)
   , InstalledFindResult (..)
   , FinderOpts(..)
   )
where

import GHC.Prelude
import GHC.Unit
import GHC.Data.OsPath
import GHC.Types.Unique.Map
import GHC.Fingerprint
import GHC.Platform.Ways
import GHC.Unit.Finder.Cache

import GHC.Data.FastString
import GHC.Types.Unique.Set

-- | The 'FinderCache' maps __home__ modules to the result of
-- searching for that module. It records the results of searching for
-- modules along the search path. On @:load@, we flush the entire
-- contents of this cache.
--
-- Searches for modules of external units are cached separately, in the
-- 'ExternalFinderCache' of the 'UnitState' being searched.
-- See Note [The finder caches] in GHC.Unit.Finder.Cache.
data FinderCache =
  FinderCache
  { homeFinderCache :: !HomeFinderCache
    -- ^ Cache of home-module searches. See 'HomeFinderCache'.
  , flushFinderCaches :: IO ()
    -- ^ Flush all home-module search caches, and also flush the file and
    -- directory hash caches.
  , lookupFileCache   :: FilePath -> IO Fingerprint
    -- ^ Look for the hash of a file in the cache. This should add it to the
    -- cache. If the file doesn't exist, raise an IOException.
  , lookupDirCache    :: FilePath -> IO Fingerprint
    -- ^ Like 'lookupFileCache', but for a directory.
  }

-- | The result of searching for an imported module.
--
-- NB: FindResult manages both user source-import lookups
-- (which can result in 'Module') as well as direct imports
-- for interfaces (which always result in 'InstalledModule').
data FindResult
  = Found ModLocation Module
        -- ^ The module was found
  | NoPackage Unit
        -- ^ The requested unit was not found
  | FoundMultiple [(Module, ModuleOrigin)]
        -- ^ _Error_: both in multiple packages

        -- | Not found
  | NotFound
      { fr_paths       :: [FilePath]       -- ^ Places where I looked

      , fr_pkg         :: Maybe Unit       -- ^ Just p => module is in this unit's
                                           --   manifest, but couldn't find the
                                           --   .hi file

      , fr_mods_hidden :: [(Unit, HiddenModuleUnitVisibility)]
                                           -- ^ Module is in these units, but
                                           --   the *module* is hidden.  The
                                           --   'HiddenModuleUnitVisibility' says whether
                                           --   the unit is itself visible.

      , fr_pkgs_hidden :: [UnitInfo]       -- ^ Module is in these units,
                                           --   but the *unit* is hidden

        -- | Module is in these units, but it is unusable
      , fr_unusables   :: [UnusableUnit]

      , fr_suggestions :: [ModuleSuggestion] -- ^ Possible mis-spelled modules
      }

-- | Locations and information the finder cares about.
--
-- Should be taken from 'DynFlags' via 'initFinderOpts'.
data FinderOpts = FinderOpts
  { finder_importPaths :: [OsPath]
      -- ^ Where are we allowed to look for Modules and Source files
  , finder_lookupHomeInterfaces :: Bool
      -- ^ When looking up a home module:
      --
      --    * 'True':  search interface files (e.g. in '-c' mode)
      --    * 'False': search source files (e.g. in '--make' mode)

  , finder_bypassHiFileCheck :: Bool
      -- ^ Don't check that an imported interface file actually exists
      -- if it can only be at one location. The interface will be reported
      -- as `InstalledFound` even if the file doesn't exist, so this is
      -- only useful in specific cases (e.g. to generate dependencies
      -- with `ghc -M`)
  , finder_ways :: Ways
  , finder_enableSuggestions :: Bool
      -- ^ If we encounter unknown modules, should we suggest modules
      -- that have a similar name.
  , finder_workingDirectory :: Maybe OsPath
  , finder_thisPackageName  :: Maybe FastString
  , finder_hiddenModules    :: !(UniqSet ModuleName)
  , finder_reexportedModules :: !(UniqMap ModuleName ModuleName) -- Reverse mapping, if you are looking for this name then look for this module.
  , finder_hieDir :: Maybe OsPath
  , finder_hieSuf :: OsString
  , finder_hiDir :: Maybe OsPath
  , finder_hiSuf :: OsString
  , finder_bytecodeDir :: Maybe OsPath
  , finder_bytecodeSuf :: OsString
  , finder_dynHiSuf :: OsString
  , finder_objectDir :: Maybe OsPath
  , finder_objectSuf :: OsString
  , finder_dynObjectSuf :: OsString
  , finder_stubDir :: Maybe OsPath
  }
