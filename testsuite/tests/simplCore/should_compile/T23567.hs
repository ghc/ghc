{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -funfolding-use-threshold=111640 -fmax-simplifier-iterations=2 #-}

module T23567 where

import T23567A

instance (MonadIO m) => CacheRWM2 (ReaderT (StateT m)) where
  p = runCacheBuildM
  {-# NOINLINE p #-}
