-- |
-- Module      : Crypto.Random.Probabilistic
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
module Crypto.Random.Probabilistic
    ( probabilistic
    ) where

import Crypto.Internal.Compat
import Crypto.Random.Types
import Crypto.Random

-- | This create a random number generator out of thin air with
-- the system entropy; don't generally use as the IO is not exposed
-- this can have unexpected random for.
-- 
-- This is useful for probabilistic algorithm like Miller Rabin
-- probably prime algorithm, given appropriate choice of the heuristic
--
-- Generally, it's advised not to use this function.
probabilistic :: MonadPseudoRandom ChaChaDRG a -> a
probabilistic f = fst $ withDRG drg f
  where {-# NOINLINE drg #-}
        drg = unsafeDoIO drgNew
{-# NOINLINE probabilistic #-}
