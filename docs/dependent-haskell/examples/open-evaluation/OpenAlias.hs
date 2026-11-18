{-# LANGUAGE NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fno-unoptimized-core-for-interpreter -fno-omit-interface-pragmas #-}
module OpenAlias (worker, route) where

-- | Return the argument through a local binding.
{-# INLINE worker #-}
worker :: u -> u
worker x = let keep = x in keep

-- | Select an operand after checking the character.
{-# INLINABLE route #-}
route :: Char -> t -> t -> t
route 'x' l _ = l
route _   _ r = r
