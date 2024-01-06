module T10069 where

data C = C !Int !Int

{-# NOINLINE c1 #-}
c1 :: C -> Int
c1 (C _ c) = c

{-# NOINLINE fc #-}
fc :: C -> Int
fc c = c1 c +  c1 c
