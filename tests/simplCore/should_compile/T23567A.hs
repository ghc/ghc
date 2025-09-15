module T23567A where

class Appl f where
  pur :: f
  ast :: f -> f

class Appl f => Mona f where
  unused :: f

class Mona f => MonadIO f where
  unused2 :: f

newtype StateT m = StateT { runStateT :: m }
  deriving (Mona, MonadIO)

instance (Appl m, Appl m) => Appl (StateT m) where
    pur = pur
    ast x = x

newtype ReaderT m = ReaderT { runReaderT :: m }
  deriving (Appl, Mona, MonadIO)

class CacheRWM2 m where
  p :: m

runCacheBuildM :: (MonadIO m) => m
runCacheBuildM = ast pur
