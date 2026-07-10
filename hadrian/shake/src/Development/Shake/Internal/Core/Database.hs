{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}

module Development.Shake.Internal.Core.Database(
    Locked, runLocked,
    DatabasePoly, createDatabase,
    mkId,
    getValueFromKey, getIdFromKey, getKeyValues, getKeyValueFromId, getKeyValuesFromId,
    setMem, setDisk, modifyAllMem
    ) where

import Data.Tuple.Extra
import Data.IORef.Extra
import General.Intern(Id, Intern)
import Development.Shake.Classes
import qualified Data.HashMap.Strict as Map
import qualified General.Intern as Intern
import Control.Concurrent.Extra
import Control.Monad.IO.Class
import qualified General.Ids as Ids
import Control.Monad.Fail
import Prelude


newtype Locked a = Locked (IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

runLocked :: DatabasePoly k v -> Locked b -> IO b
runLocked db (Locked act) = withLock (lock db) act


-- | Invariant: The database does not have any cycles where a Key depends on itself.
--   Everything is mutable. intern and status must form a bijection.
--   There may be dangling Id's as a result of version changes.
--   Lock is used to prevent any torn updates
data DatabasePoly k v = Database
    {lock :: Lock
    ,intern :: IORef (Intern k) -- ^ Key |-> Id mapping
    ,status :: Ids.Ids (k, v) -- ^ Id |-> (Key, Status) mapping
    ,journal :: Id -> k -> v -> IO () -- ^ Record all changes to status
    ,vDefault :: v
    }


createDatabase
    :: (Eq k, Hashable k)
    => Ids.Ids (k, v)
    -> (Id -> k -> v -> IO ())
    -> v
    -> IO (DatabasePoly k v)
createDatabase status journal vDefault = do
    xs <- Ids.toList status
    intern <- newIORef $ Intern.fromList [(k, i) | (i, (k,_)) <- xs]
    lock <- newLock
    pure Database{..}


---------------------------------------------------------------------
-- SAFE READ-ONLY

getValueFromKey :: (Eq k, Hashable k) => DatabasePoly k v -> k -> IO (Maybe v)
getValueFromKey Database{..} k = do
    is <- readIORef intern
    case Intern.lookup k is of
        Nothing -> pure Nothing
        Just i -> fmap snd <$> Ids.lookup status i

-- Returns Nothing only if the Id was serialised previously but then the Id disappeared
getKeyValueFromId :: DatabasePoly k v -> Id -> IO (Maybe (k, v))
getKeyValueFromId Database{..} = Ids.lookup status

getKeyValues :: DatabasePoly k v -> IO [(k, v)]
getKeyValues Database{..} = Ids.elems status

getKeyValuesFromId :: DatabasePoly k v -> IO (Map.HashMap Id (k, v))
getKeyValuesFromId Database{..} = Ids.toMap status

getIdFromKey :: (Eq k, Hashable k) => DatabasePoly k v -> IO (k -> Maybe Id)
getIdFromKey Database{..} = do
    is <- readIORef intern
    pure $ flip Intern.lookup is


---------------------------------------------------------------------
-- MUTATING

-- | Ensure that a Key has a given Id, creating an Id if there is not one already
mkId :: (Eq k, Hashable k) => DatabasePoly k v -> k -> Locked Id
mkId Database{..} k = liftIO $ do
    is <- readIORef intern
    case Intern.lookup k is of
        Just i -> pure i
        Nothing -> do
            (is, i)<- pure $ Intern.add k is
            -- make sure to write it into Status first to maintain Database invariants
            Ids.insert status i (k, vDefault)
            writeIORef' intern is
            pure i


setMem :: DatabasePoly k v -> Id -> k -> v -> Locked ()
setMem Database{..} i k v = liftIO $ Ids.insert status i (k,v)

modifyAllMem :: DatabasePoly k v -> (v -> v) -> Locked ()
modifyAllMem Database{..} f = liftIO $ Ids.forMutate status $ \(k,v) ->
    let !v' = f v
    in (k, v')

setDisk :: DatabasePoly k v -> Id -> k -> v -> IO ()
setDisk = journal
