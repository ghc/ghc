{-# LANGUAGE DataKinds #-}
module T17366_ARa where

import Control.Monad.IO.Class
import Data.Kind

type Effect = (Type -> Type) -> Type -> Type

data Env (es :: [Effect]) = Env

newtype Eff (es :: [Effect]) a = Eff { unEff :: Env es -> IO a }
  deriving Functor

instance Applicative (Eff es) where
  pure a = Eff $ \_ -> pure a
  f <*> a = Eff $ \es -> unEff f es <*> unEff a es

instance Monad (Eff es) where
  m >>= f = Eff $ \es -> unEff m es >>= (`unEff` es) . f

instance MonadIO (Eff es) where
  liftIO m = Eff $ \_ -> m

----------------------------------------

smallTest :: MonadIO m => m ()
smallTest = do
  liftIO $ putStrLn "test"
  liftIO $ putStrLn "test"
  liftIO $ putStrLn "test"
  liftIO $ putStrLn "test"
  liftIO $ putStrLn "test"
{-# INLINABLE smallTest #-} -- When uncommented, smallTestSpec no longer uses specialized smallTest.

test :: MonadIO m => m ()
test = do
  liftIO $ putStrLn "test"
  liftIO $ putStrLn "test"
  liftIO $ putStrLn "test"
  liftIO $ putStrLn "test"
  liftIO $ putStrLn "test"
  liftIO $ putStrLn "test"
  liftIO $ putStrLn "test"
  liftIO $ putStrLn "test"
  liftIO $ putStrLn "test"
  liftIO $ putStrLn "test"
  liftIO $ putStrLn "test"
  liftIO $ putStrLn "test"
  liftIO $ putStrLn "test"
  liftIO $ putStrLn "test"
  liftIO $ putStrLn "test"
  liftIO $ putStrLn "test"
  liftIO $ putStrLn "test"
  liftIO $ putStrLn "test"
  liftIO $ putStrLn "test"
  liftIO $ putStrLn "test"
{-# INLINABLE test #-}
