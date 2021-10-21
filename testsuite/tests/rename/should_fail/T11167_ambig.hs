{-# LANGUAGE DuplicateRecordFields #-}
module T11167_ambig where

data SomeExceptionWithLocation

newtype ContT r m a = ContT {runContT :: (a -> m r) -> m r}
newtype ContT' r m a = ContT' {runContT :: (a -> m r) -> m r}

runContT' :: ContT r m a -> (a -> m r) -> m r
runContT' = runContT

catch_ :: IO a -> (SomeExceptionWithLocation -> IO a) -> IO a
catch_ = undefined

foo :: IO ()
foo = (undefined :: ContT () IO a)
        `runContT` (undefined :: a -> IO ())
        `catch_` (undefined :: SomeExceptionWithLocation -> IO ())

foo' :: IO ()
foo' = (undefined :: ContT () IO a)
         `runContT'` (undefined :: a -> IO ())
         `catch_` (undefined :: SomeExceptionWithLocation -> IO ())
