module GHC.Unit.External.Database (
  -- * Mutable cache for 'ExternalUnitDatabases'
  ExternalUnitDatabaseCache (..),
  initExternalUnitDatabaseCache,
  readExternalUnitDatabases,
  readExternalUnitDatabase,
  cacheExternalUnitDatabase,
  clearExternalUnitDatabaseCache,
  -- * 'ExternalUnitDatabases'
  ExternalUnitDatabases,
  emptyExternalUnitDatabases,
  insertExternalUnitDatabases,
  deleteExternalUnitDatabases,
  lookupExternalUnitDatabases,
  -- * 'UnitDatabase'
  UnitDatabase (..),
) where

import GHC.Prelude

import GHC.Data.OsPath
import GHC.Unit.Info
import GHC.Utils.Outputable

import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Map.Strict
import Data.Map.Strict qualified as Map

-- ----------------------------------------------------------------------------
-- ExternalUnitDatabaseCache
-- ----------------------------------------------------------------------------

newtype ExternalUnitDatabaseCache unit = ExternalUnitDatabaseCache
  { eudc_databases :: IORef (ExternalUnitDatabases unit)
  }

initExternalUnitDatabaseCache :: IO (ExternalUnitDatabaseCache unit)
initExternalUnitDatabaseCache =
  ExternalUnitDatabaseCache <$> IORef.newIORef emptyExternalUnitDatabases

readExternalUnitDatabases :: ExternalUnitDatabaseCache unit -> IO (ExternalUnitDatabases unit)
readExternalUnitDatabases eudc =
  IORef.readIORef (eudc_databases eudc)

modifyExternalUnitDatabaseCache :: ExternalUnitDatabaseCache unit -> (ExternalUnitDatabases unit -> ExternalUnitDatabases unit) -> IO ()
modifyExternalUnitDatabaseCache eudc f =
  IORef.modifyIORef' (eudc_databases eudc) f

readExternalUnitDatabase :: ExternalUnitDatabaseCache unit -> OsPath -> IO (Maybe (UnitDatabase unit))
readExternalUnitDatabase eudc path = do
  dbs <- readExternalUnitDatabases eudc
  pure $ lookupExternalUnitDatabases path dbs

cacheExternalUnitDatabase :: ExternalUnitDatabaseCache unit -> UnitDatabase unit -> IO ()
cacheExternalUnitDatabase eudc db =
  modifyExternalUnitDatabaseCache eudc (insertExternalUnitDatabases db)

clearExternalUnitDatabaseCache :: ExternalUnitDatabaseCache unit -> IO ()
clearExternalUnitDatabaseCache eudc =
  modifyExternalUnitDatabaseCache eudc (const emptyExternalUnitDatabases)

-- ----------------------------------------------------------------------------
-- ExternalUnitDatabases
-- ----------------------------------------------------------------------------

data ExternalUnitDatabases unit = ExternalUnitDatabases
  { eud_cachedDatabases :: !(Map OsPath (UnitDatabase unit))
  }

emptyExternalUnitDatabases :: ExternalUnitDatabases unit
emptyExternalUnitDatabases =
  ExternalUnitDatabases
    { eud_cachedDatabases = Map.empty
    }

insertExternalUnitDatabases :: UnitDatabase unit -> ExternalUnitDatabases unit -> ExternalUnitDatabases unit
insertExternalUnitDatabases unit_db eud =
  ExternalUnitDatabases
    { eud_cachedDatabases = Map.insert (unitDatabasePath unit_db) unit_db (eud_cachedDatabases eud)
    }

deleteExternalUnitDatabases :: OsPath -> ExternalUnitDatabases unit -> ExternalUnitDatabases unit
deleteExternalUnitDatabases unit_db_path eud =
  ExternalUnitDatabases
    { eud_cachedDatabases = Map.delete unit_db_path (eud_cachedDatabases eud)
    }

lookupExternalUnitDatabases :: OsPath -> ExternalUnitDatabases unit -> Maybe (UnitDatabase unit)
lookupExternalUnitDatabases key eud =
  Map.lookup key (eud_cachedDatabases eud)

-- ----------------------------------------------------------------------------
-- UnitDatabase
-- ----------------------------------------------------------------------------

-- | Unit database
data UnitDatabase unit = UnitDatabase
  { unitDatabasePath :: OsPath
  , unitDatabaseUnits :: [GenUnitInfo unit]
  }

instance (Outputable u) => Outputable (UnitDatabase u) where
  ppr (UnitDatabase fp _u) = text "DB:" <+> ppr fp
