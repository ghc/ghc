module T14287 (filterTest) where

import GHC.Exts

data Stream s a = Stream (s -> Step s a) s
data Step s a = Done | Yield a s

sfilter :: (a -> Bool) -> Stream s a -> Stream s a
sfilter pred (Stream step s0) = Stream filterStep s0 where
  filterStep s = case step s of
    Done -> Done
    Yield x ns
      | pred x    -> Yield x ns
      | otherwise -> filterStep ns

fromTo :: Int -> Int -> Stream Int Int
{-# INLINE fromTo #-}
fromTo from to = Stream step from where
  step i
    | i > to    = Done
    | otherwise = Yield i (i + 1)

sfoldl :: (b -> a -> b) -> b -> Stream s a -> b
{-# INLINE sfoldl #-}
sfoldl acc z (Stream !step s0) = oneShot go z s0 where
  go !y s = case step s of
    Done       -> y
    Yield x ns -> go (acc y x) ns

ssum :: (Num a) => Stream s a -> a
ssum = sfoldl (+) 0

filterTest :: Int
filterTest = ssum $ sfilter even (fromTo 1 101)
