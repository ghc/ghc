module GHC.Unit.Finder.Types
   ( FinderCache (..)
   , FinderCacheState
   , FileCacheState
   , FindResult (..)
   , InstalledFindResult (..)
   , FinderOpts(..)
   )
where

import GHC.Prelude
import GHC.Unit
import GHC.Data.OsPath
import qualified Data.Map as M
import GHC.Fingerprint
import GHC.Platform.Ways
import GHC.Unit.Env

import GHC.Data.FastString
import qualified Data.Set as Set

-- | The 'FinderCache' maps modules to the result of
-- searching for that module. It records the results of searching for
-- modules along the search path. On @:load@, we flush the entire
-- contents of this cache.
--
type FinderCacheState = InstalledModuleEnv InstalledFindResult
type FileCacheState   = M.Map FilePath Fingerprint
data FinderCache = FinderCache { flushFinderCaches :: UnitEnv -> IO ()
                               -- ^ remove all the home modules from the cache; package modules are
                               -- assumed to not move around during a session; also flush the file hash
                               -- cache.
                               , addToFinderCache  :: InstalledModuleWithIsBoot -> InstalledFindResult -> IO ()
                               -- ^ Add a found location to the cache for the module.
                               , lookupFinderCache :: InstalledModuleWithIsBoot -> IO (Maybe InstalledFindResult)
                               -- ^ Look for a location in the cache.
                               , lookupFileCache   :: FilePath -> IO Fingerprint
                               -- ^ Look for the hash of a file in the cache. This should add it to the
                               -- cache. If the file doesn't exist, raise an IOException.
                               }

data InstalledFindResult
  = InstalledFound ModLocation InstalledModule
  | InstalledNoPackage UnitId
  | InstalledNotFound [OsPath] (Maybe UnitId)

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

      , fr_mods_hidden :: [Unit]           -- ^ Module is in these units,
                                           --   but the *module* is hidden

      , fr_pkgs_hidden :: [Unit]           -- ^ Module is in these units,
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
  , finder_hiddenModules    :: Set.Set ModuleName
  , finder_reexportedModules :: M.Map ModuleName ModuleName -- Reverse mapping, if you are looking for this name then look for this module.
  , finder_hieDir :: Maybe OsPath
  , finder_hieSuf :: OsString
  , finder_hiDir :: Maybe OsPath
  , finder_hiSuf :: OsString
  , finder_dynHiSuf :: OsString
  , finder_objectDir :: Maybe OsPath
  , finder_objectSuf :: OsString
  , finder_dynObjectSuf :: OsString
  , finder_stubDir :: Maybe OsPath
  }
