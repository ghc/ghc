{-# OPTIONS -ddump-parsed-ast #-}
module Test20297 where


bar = x
  -- comment0
  where -- comment1

foo = x
  where -- comment2
        doStuff = do stuff
