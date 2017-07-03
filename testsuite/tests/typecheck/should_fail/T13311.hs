{-# LANGUAGE RankNTypes #-}
module T13311 where

f :: forall a. (Monoid a) => forall b. (Monoid b) => Maybe a -> Maybe b
f _ = mempty

g :: IO ()
g = do
  f
  putChar 'a'
