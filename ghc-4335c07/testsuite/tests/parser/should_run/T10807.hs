{-# LANGUAGE MultiWayIf #-}

module Main where

-- This is how we had to use multi-way if previously. Not indenting lines after
-- `|` was causing a parse error.
f1 x = if | even x
           , x /= 0
           -> True
          | otherwise
           -> False

-- This was previously causing a parse error, but actually it should work.
f2 x = if | even x
          , x /= 0
          -> True
          | otherwise
          -> False

-- If we don't generate {} in MultiWayIf we get a shift/reduce conflict here:
-- It's not clear which guards belong to `case` and which ones belong to `if`.
--
-- This test is to make sure we parse it correctly.
--
-- - If we shift, we get a non-exhaustive pattern error when argument is odd.
-- - If we reduce, we run the unreachable code when argument is odd.
f3 x = case x of
         x' | even x'   -> if | even x' -> 1 | otherwise -> error "should be unreachable"
            | otherwise -> 3

-- Testing line breaks
f4 x = case x of
         x' | even x'   -> if
             | even x' -> 1
             | otherwise -> error "should be unreachable"
            | otherwise -> 3

main :: IO ()
main = do
  print (f3 1)
  print (f3 2)
  print (f4 1)
  print (f4 2)
