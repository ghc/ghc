module T11167 where

data SomeExceptionWithBacktrace

newtype ContT r m a = ContT {runContT :: (a -> m r) -> m r}

runContT' :: ContT r m a -> (a -> m r) -> m r
runContT' = runContT

catch_ :: IO a -> (SomeExceptionWithBacktrace -> IO a) -> IO a
catch_ = undefined

foo :: IO ()
foo = (undefined :: ContT () IO a)
        `runContT` (undefined :: a -> IO ())
        `catch_` (undefined :: SomeExceptionWithBacktrace -> IO ())

foo' :: IO ()
foo' = (undefined :: ContT () IO a)
         `runContT'` (undefined :: a -> IO ())
         `catch_` (undefined :: SomeExceptionWithBacktrace -> IO ())
