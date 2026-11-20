{-# LANGUAGE CPP #-}
{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Modifiers #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -fomit-interface-pragmas -fno-write-if-simplified-core #-}

-- | The payload projection for a retained dependent pair.
module GHC.Sigma.Projection (second) where

import GHC.Sigma (Sigma(Pair), first)
#if __GLASGOW_HASKELL__ >= 1001
import qualified GHC.Essentials as Rebindable
#endif

-- | Return the payload at the pair's witness.
second :: foreach (q :: Sigma a p) -> p (first q)
second (Pair _ value) = value
