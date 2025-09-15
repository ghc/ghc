{-# LANGUAGE MagicHash #-}
module T24551 (f) where

import GHC.Exts

f :: a -> a
f = repeatFB g

repeatFB :: (Addr# -> (a -> a) -> a -> a) -> a -> a
repeatFB c = let xs = c "missing"# xs in xs
{-# INLINE [0] repeatFB #-}

g :: Addr# -> (a -> a) -> a -> a
g _ _ x = x
{-# NOINLINE g #-}
