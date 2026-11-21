{-# LANGUAGE ExplicitNamespaces, GADTs, ScopedTypeVariables, TypeAbstractions, ViewPatterns #-}
module GadtOrderedTelescopeScope where

import GadtOrderedTelescopeProvider (FieldCheck(..), Mixed(..), RequiredFieldCheck(..))

wrong :: Mixed a -> Bool
wrong (Mixed n @a x @b) = x == x

-- | The first field occurs before the equality context.
wrongEarly :: FieldCheck a -> Bool
wrongEarly (FieldCheck ((\x -> x == x) -> result) _) = result

data Fenced a where
  Fenced :: Ord a => (a, a) -> (a ~ Int) => Fenced a

badField :: Fenced a -> Int
badField (Fenced (x :: (Int, Int))) = fst x

wrongRequiredEarly :: RequiredFieldCheck -> Bool
wrongRequiredEarly (RequiredFieldCheck _ (type a) ((\x -> x == x) -> result) _) = result
