{-# LANGUAGE MagicHash #-}

import GHC.Exts

{-# NOINLINE ix #-}
ix :: Int
ix = 0

{-# NOINLINE f #-}
f :: Int8# -> Int#
f x = if isTrue# (x `ltInt8#` intToInt8# 0#)
        then (int8ToWord8# x) `gtWord8#` wordToWord8# 200##
        else 1#

main :: IO ()
main = do
  -- Test for use of byte-width read.
  let !(I# i) = ix
      x = indexInt8OffAddr# "\x80"# i
  putStrLn ("f(0x80) = " ++ show (I# (f x)))
