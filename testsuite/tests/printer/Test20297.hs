{-# OPTIONS -ddump-parsed-ast #-}
{-# LANGUAGE MultiWayIf #-}

module Test20297 where


bar = x
  -- comment0
  where -- comment1

foo = x
  where -- comment2
        doStuff = do stuff

-- Layout in multi-if
f = if | x -> y
       | z -> w

-- eof
