{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module GHC25266 where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, mapReaderT, runReaderT)
import Data.Kind (Type)
import Data.Void (Void)
import GHC.Stack (HasCallStack, withFrozenCallStack)

class MonadIO m => CanRunDB m where
  unsafeUnlabelledRunDB :: HasCallStack => SqlPersistT m a -> m a

type DBImpl backend env = ReaderT env (ReaderT backend IO)

newtype DBWith backend env a = DB (DBImpl backend env a)
  deriving newtype (Functor, Applicative, Monad)

type DBEnv = ()

type DB = DBWith SqlBackend DBEnv

class Monad m => PersistentOperation m where
  type PersistentBackend m
  unsafeLiftPersistentOperation :: HasCallStack => ReaderT (PersistentBackend m) IO a -> m a

instance PersistentOperation (DBWith backend env) where
  type PersistentBackend (DBWith backend env) = backend
  unsafeLiftPersistentOperation = DB . lift . checkpointCallStack

toSqlPersistTIO :: env -> DBWith backend env a -> ReaderT backend IO a
toSqlPersistTIO env (DB act) = runReaderT act env

hoistIO :: MonadIO m => ReaderT backend IO a -> ReaderT backend m a
hoistIO = mapReaderT liftIO

liftToSqlPersistT :: forall m a backend. (CanRunDB m) => DBWith backend DBEnv a -> ReaderT backend m a
liftToSqlPersistT action = do
  let dbEnv = ()
  hoistIO $ toSqlPersistTIO dbEnv action

runDB :: (HasCallStack, CanRunDB m) => DB a -> m a
runDB action = withFrozenCallStack unsafeUnlabelledRunDB $ liftToSqlPersistT action

streamRows ::
  forall m a.
  (MonadUnliftIO m, CanRunDB m) =>
  (forall n. (PersistentOperation n, PersistentBackend n ~ SqlBackend) => n [a]) ->
  ConduitT () [a] m ()
streamRows runQuery = go (10 :: Integer)
  where
    go n
      | n < 0 = pure ()
      | otherwise = do
          rows <- lift . runDB $ runQuery
          yield rows
          go (n - 1)

expectedList :: [Int]
expectedList = [1, 2, 3]

query :: forall n. (PersistentOperation n, PersistentBackend n ~ SqlBackend) => n [Int]
query = pure expectedList

test_success :: forall m. (MonadUnliftIO m, CanRunDB m) => m [[Int]]
test_success = do
  let conduit = streamRows query .| (sinkList @_ @[Int])
  runConduit conduit

test_fail :: forall m. (MonadUnliftIO m, CanRunDB m) => m [[Int]]
test_fail = do
  let conduit = streamRows query .| sinkList
  runConduit conduit

-----
-- annotated-exception
-----

checkpointCallStack
    -- :: (MonadCatch m, HasCallStack)
    :: (Monad m, HasCallStack)
    => m a
    -> m a
checkpointCallStack = id

-----
-- conduit
-----

data ConduitT i o (m :: Type -> Type) r
instance Functor (ConduitT i o m)
instance Applicative (ConduitT i o m)
instance Monad (ConduitT i o m)
instance MonadTrans (ConduitT i o)

(.|) :: Monad m => ConduitT a b m () -> ConduitT b c m r -> ConduitT a c m r
(.|) = undefined

runConduit :: Monad m => ConduitT () Void m r -> m r
runConduit = undefined

sinkList :: Monad m => ConduitT a o m [a]
sinkList = undefined

yield :: Monad m => o -> ConduitT i o m ()
yield = undefined

-----
-- persistent
-----

data SqlBackend

type SqlPersistT = ReaderT SqlBackend

-----
-- unliftio
-----

class MonadIO m => MonadUnliftIO m where
  withRunInIO :: ((forall a. m a -> IO a) -> IO b) -> m b
