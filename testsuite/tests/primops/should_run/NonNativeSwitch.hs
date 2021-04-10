{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import GHC.Exts
import GHC.Word (Word8(..), Word16(..), Word32(..))

-- We expect these ifs to be rewritten into core switches. Then they
-- will become C-- switches possibly after ifs again. Either way, C--
-- switches only supported native comparisons before, so this tests that
-- non-native ones work.

main :: IO ()
main = do
  let W8# five = 5
  switch8 five
  let W16# five = 5
  switch16 five
  let W32# five = 5
  switch32 five

{-# NOINLINE switch8 #-}
switch8 :: Word8# -> IO ()
switch8 n =
    if isTrue# (n `eqWord8#` three)
    then putStrLn "Word8 is 3"
    else putStrLn "Word8 is not 3"
  where
      W8# three = 3

{-# NOINLINE switch16 #-}
switch16 :: Word16# -> IO ()
switch16 n =
    if isTrue# (n `eqWord16#` three)
    then putStrLn "Word16 is 3"
    else putStrLn "Word16 is not 3"
  where
      W16# three = 3

{-# NOINLINE switch32 #-}
switch32 :: Word32# -> IO ()
switch32 n =
    if isTrue# (n `eqWord32#` three)
    then putStrLn "Word32 is 3"
    else putStrLn "Word32 is not 3"
  where
      W32# three = 3
