module T9136 where

import Data.Word

-- In all these example, no 8 should be found in the final code
foo1 :: Int -> Int
foo1 x = (x + 8) - 1

foo2 :: Int -> Int
foo2 x = (8 + x) - 2

foo3 :: Int -> Int -> Int
foo3 x y = ((8 + x) + y) - 2

foo4 :: Int -> Int -> Int
foo4 x y = (8 + x) + (y - 3)

word1 :: Word -> Word
word1 x = (x + 8) + 1

word2 :: Word -> Word
word2 x = (8 + x) + 2

word3 :: Word -> Word -> Word
word3 x y = ((8 + x) + y) + 2

word4 :: Word -> Word -> Word
word4 x y = (8 + x) + (y + 3)
