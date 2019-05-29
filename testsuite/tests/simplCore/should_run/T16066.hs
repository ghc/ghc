{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Monad (join)
import Control.Monad.Reader (ReaderT(..))
import Control.Concurrent.STM (STM, atomically)
import Data.Kind (Type)

class Monad (Transaction m) => MonadPersist m where
  type Transaction m :: Type -> Type
  atomicTransaction :: Transaction m y -> m y

instance MonadPersist (ReaderT () IO) where
  type Transaction (ReaderT () IO) = ReaderT () STM
  atomicTransaction act = ReaderT (atomically . runReaderT act)

main :: IO ()
main = join (runReaderT doPure2 ()) >>= \x -> seq x (return ())

doPure2 :: MonadPersist m => m (IO ())
doPure2 = atomicTransaction $ do
  () <- pure ()
  () <- pure ()
  error "exit never happens"
