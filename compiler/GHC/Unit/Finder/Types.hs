module GHC.Unit.Finder.Types
   ( FinderCache (..)
   , FinderCacheState
   , FindResult (..)
   , InstalledFindResult (..)
   , FinderOpts(..)
   )
where

import GHC.Prelude
import GHC.Unit
import qualified Data.Map as M
import GHC.Fingerprint
import GHC.Platform.Ways

import Data.IORef
import GHC.Data.FastString
import qualified Data.Set as Set

-- | The 'FinderCache' maps modules to the result of
-- searching for that module. It records the results of searching for
-- modules along the search path. On @:load@, we flush the entire
-- contents of this cache.
--
type FinderCacheState = InstalledModuleEnv InstalledFindResult
type FileCacheState   = M.Map FilePath Fingerprint
data FinderCache = FinderCache { fcModuleCache :: (IORef FinderCacheState)
                               , fcFileCache   :: (IORef FileCacheState)
                               }

data InstalledFindResult
  = InstalledFound ModLocation InstalledModule
  | InstalledNoPackage UnitId
  | InstalledNotFound [FilePath] (Maybe UnitId)

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
      , fr_unusables   :: [(Unit, UnusableUnitReason)]

      , fr_suggestions :: [ModuleSuggestion] -- ^ Possible mis-spelled modules
      }

-- | Locations and information the finder cares about.
--
-- Should be taken from 'DynFlags' via 'initFinderOpts'.
data FinderOpts = FinderOpts
  { finder_importPaths :: [FilePath]
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
  , finder_workingDirectory :: Maybe FilePath
  , finder_thisPackageName  :: Maybe FastString
  , finder_hiddenModules    :: Set.Set ModuleName
  , finder_reexportedModules :: Set.Set ModuleName
  , finder_hieDir :: Maybe FilePath
  , finder_hieSuf :: String
  , finder_hiDir :: Maybe FilePath
  , finder_hiSuf :: String
  , finder_dynHiSuf :: String
  , finder_objectDir :: Maybe FilePath
  , finder_objectSuf :: String
  , finder_dynObjectSuf :: String
  , finder_stubDir :: Maybe FilePath
  } deriving Show
