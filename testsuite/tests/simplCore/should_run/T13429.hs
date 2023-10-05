{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import T13429a

import Data.Foldable (Foldable(..))
import Data.Monoid (Monoid(..))

main :: IO ()
main = print $ prop_mappend z z
  where
    z :: Seq Integer
    z = deep (Four 1 2 3 4) Empty (Four 1 2 3 4)

infix 4 ~=

(~=) :: Eq a => Maybe a -> a -> Bool
(~=) = maybe (const False) (==)

-- Partial conversion of an output sequence to a list.
toList' :: (Eq a, Measured [a] a, Valid a) => Seq a -> Maybe [a]
toList' xs
  | valid xs = Just (toList xs)
  | otherwise = Nothing

prop_mappend :: Seq Integer -> Seq Integer -> Bool
prop_mappend xs ys =
    toList' (mappend xs ys) ~= toList xs ++ toList ys

------------------------------------------------------------------------
-- Valid trees
------------------------------------------------------------------------

class Valid a where
    valid :: a -> Bool

instance (Measured v a, Eq v, Valid a) => Valid (FingerTree v a) where
    valid Empty = True
    valid (Single x) = valid x
    valid (Deep s pr m sf) =
        s == measure pr `mappend` measure m `mappend` measure sf &&
        valid pr && valid m && valid sf

instance (Measured v a, Eq v, Valid a) => Valid (Node v a) where
    valid node = measure node == foldMap measure node && all valid node

instance Valid a => Valid (Digit a) where
    valid = all valid

instance Valid Integer where
    valid = const True

------------------------------------------------------------------------
-- Use list of elements as the measure
------------------------------------------------------------------------

type Seq a = FingerTree [a] a

instance Measured [Integer] Integer where
    measure x = [x]
