{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import GHC.Exts
import GHC.Types
import GHC.Num

data T = T Word8#

instance Num T where
  fromInteger i = T (wordToWord8# (integerToWord# i))

instance Eq T where
  (==) (T a) (T b) = isTrue# (a `eqWord8#` b)

main :: IO ()
main = do
  let !addr = "\0\1\2\3\4\5\6\7\8"#

  w8 <- IO (\s -> case readWord8OffAddr# (plusAddr# addr 5#) 0# s of
                      (# s', w8 #) -> (# s', T w8 #))
  -- w8 must be small enough for one of the branch to be taken.
  -- we need several alternatives for a jump table to be used
  print $ case w8 of
      0 -> 1000
      1 -> 1001
      2 -> 1002
      3 -> 1003
      4 -> 1004
      5 -> 1005
      6 -> 1006
      _ -> 1010
