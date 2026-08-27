module GHC.Unit.Finder.Types
   ( FinderCache (..)
   , FindResult (..)
   , InstalledFindResult (..)
   , FinderOpts(..)
     -- * Known home modules
   , KnownHomeModules
   , emptyKnownHomeModules
   , mkKnownHomeModules
   , lookupKnownHomeModule
   , knownHomeModulesOfSummaries
   , knownHomeModulesOfGraph
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
import GHC.Unit.Module.Graph ( ModuleGraph, mgModSummaries )
import GHC.Unit.Module.ModSummary ( ModSummary(ms_location), ms_mod, isBootSummary )

import qualified Data.Semigroup ( (<>) )

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
  { knownHomeModules :: KnownHomeModules
    -- ^ Home modules whose defining file is known ahead of any search,
    -- overriding the search.
    --
    -- See Note [Known home modules].
  , homeFinderCache :: !HomeFinderCache
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

{- Note [Known home modules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Most home modules are found by searching the import paths for a file whose
filename matches the module name. However, a file target is allowed to live
outside the search paths, and its module name is allowed to differ from the
file name. Such module names can therefore only be resolved by consulting
the file's parsed header, and never by search.

'KnownHomeModules' holds exactly this information, as an immutable value.
This is in contrast to the search memos of the 'FinderCache', which may be
extended at any time but only ever memoise the pure search function.
-}

-- | Home modules whose defining file is known ahead of time, overriding
-- the file search. See Note [Known home modules].
newtype KnownHomeModules = KnownHomeModules (InstalledModuleEnv ModLocation)

emptyKnownHomeModules :: KnownHomeModules
emptyKnownHomeModules = KnownHomeModules emptyInstalledModuleEnv

-- | Left-biased: on a duplicate module, the earlier entry wins.
mkKnownHomeModules :: [(InstalledModule, ModLocation)] -> KnownHomeModules
mkKnownHomeModules entries =
  KnownHomeModules $
    foldr (\ (m, loc) env -> extendInstalledModuleEnv env m loc)
      emptyInstalledModuleEnv entries

lookupKnownHomeModule :: KnownHomeModules -> InstalledModule -> Maybe ModLocation
lookupKnownHomeModule (KnownHomeModules env) mod =
  lookupInstalledModuleEnv env mod

-- | Left-biased.
instance Semigroup KnownHomeModules where
  KnownHomeModules l <> KnownHomeModules r =
    KnownHomeModules (plusInstalledModuleEnv (\ left _right -> left) l r)

instance Monoid KnownHomeModules where
  mempty = emptyKnownHomeModules

-- | The known home modules of the given summaries (left-biased).
-- See Note [Known home modules].
knownHomeModulesOfSummaries :: [ModSummary] -> KnownHomeModules
knownHomeModulesOfSummaries summaries =
  mkKnownHomeModules
    [ (toUnitId <$> ms_mod ms, ms_location ms)
    | ms <- summaries
    , NotBoot <- [isBootSummary ms]
    ]

-- | The known home modules of a module graph's summaries.
-- See Note [Known home modules].
--
-- Beware: this forces the module graph, so it must not be used on a lazily
-- built graph (such as the one-shot graph of 'GHC.Driver.Downsweep.downsweepThunk').
knownHomeModulesOfGraph :: ModuleGraph -> KnownHomeModules
knownHomeModulesOfGraph = knownHomeModulesOfSummaries . mgModSummaries

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
