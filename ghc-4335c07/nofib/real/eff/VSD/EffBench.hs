{-# LANGUAGE RankNTypes, BangPatterns, ScopedTypeVariables,
  MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

{-# OPTIONS -fexpose-all-unfoldings #-}



module EffBench where

import qualified Control.Monad.State.Strict as S


times :: Monad m => Int -> m a -> m ()
times n ma = go n where
  go 0 = pure ()
  go n = ma >> go (n - 1)
{-# inline times #-}

-- classy van Laarhoven state (same as mtl)
--------------------------------------------------------------------------------

class VPutD s m where vput :: s -> m ()
class VGetD s m where vget :: m s

vmdmodify :: (Monad m, VPutD s m, VGetD s m) => (s -> s) -> m ()
vmdmodify f = do
  s <- vget
  let !s' = f s
  vput s'

instance VPutD s (S.State s) where vput = S.put
instance VGetD s (S.State s) where vget = S.get



