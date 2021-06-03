{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Stream.StreamD.Step
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Step
    (
    -- * The stream type
      Step (..)
    )
where

#if defined(FUSION_PLUGIN)
import Fusion.Plugin.Types (Fuse(..))
#endif

-- | A stream is a succession of 'Step's. A 'Yield' produces a single value and
-- the next state of the stream. 'Stop' indicates there are no more values in
-- the stream.
#if defined(FUSION_PLUGIN)
{-# ANN type Step Fuse #-}
#endif
data Step s a = Yield a s | Skip s | Stop

instance Functor (Step s) where
    {-# INLINE fmap #-}
    fmap f (Yield x s) = Yield (f x) s
    fmap _ (Skip s) = Skip s
    fmap _ Stop = Stop

{-
yield :: Monad m => a -> s -> m (Step s a)
yield a = return . Yield a

skip :: Monad m => s -> m (Step s a)
skip = return . Skip

stop :: Monad m => m (Step s a)
stop = return Stop
-}
