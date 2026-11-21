{-# LANGUAGE GADTs, TypeAbstractions, ViewPatterns #-}
module GadtOrderedTelescopeScope where

import GadtOrderedTelescopeProvider (FieldCheck(..), Mixed(..))

wrong :: Mixed a -> Bool
wrong (Mixed n @a x @b) = x == x

-- | The first field occurs before the equality context.
wrongEarly :: FieldCheck a -> Bool
wrongEarly (FieldCheck ((\x -> x == x) -> result) _) = result
