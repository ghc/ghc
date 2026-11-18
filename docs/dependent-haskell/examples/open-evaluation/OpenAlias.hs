{-# LANGUAGE NoExperimentalDependentTypes #-}
module OpenAlias (worker) where

-- | Return the argument through a local binding.
{-# INLINE worker #-}
worker :: u -> u
worker x = let keep = x in keep
