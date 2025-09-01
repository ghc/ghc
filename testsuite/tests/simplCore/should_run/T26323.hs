module Main where

f :: Int -> Int
f x = g x
{-# INLINE [1] f #-}

g :: Int -> Int
g x = 0
{-# NOINLINE g #-}

h :: Int -> Int
h _ = 1
{-# NOINLINE h #-}

{-# RULES "r1" [2]  forall x. g x = h x #-}
{-# RULES "r2" [~1] forall x. h x = 2 #-}

test :: Int
test = f 3

main :: IO ()
main = print test
  --  we should get
  --
  --  f 3
  --    ==> inline in phase 1
  --  g 3
  --    ==> use 'r1' in phase 1
  --  h 3
  --    = 1
  --
  -- Here rule 'r2' should never fire, so we SHOULD NOT rewrite 'h 3' to '2'.
