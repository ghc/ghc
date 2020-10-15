module GHC.Unit.Finder.Types
   ( FinderCache
   , FindResult (..)
   , InstalledFindResult (..)
   )
where

import GHC.Prelude
import GHC.Unit
import GHC.Unit.State

-- | The 'FinderCache' maps modules to the result of
-- searching for that module. It records the results of searching for
-- modules along the search path. On @:load@, we flush the entire
-- contents of this cache.
--
type FinderCache = InstalledModuleEnv InstalledFindResult

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

