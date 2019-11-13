{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import GHC.Exts

main :: IO ()
main = do
  putStr "A: "
  print $
    I# (cstringLength# "hello_world"#)
    ==
    naiveStrlen "hello_world"# 0
  putStr "B: "
  print $
    I# (cstringLength# "aaaaaaaaaaaaa\x00b"#)
    ==
    naiveStrlen "aaaaaaaaaaaaa\x00b"# 0
  putStr "C: "
  print $
    I# (cstringLength# "cccccccccccccccccc\x00b"#)
    ==
    naiveStrlen "cccccccccccccccccc\x00b"# 0
  putStr "D: "
  print $
    I# (cstringLength# "araña\NULb"#)
    ==
    naiveStrlen "araña\NULb"# 0

naiveStrlen :: Addr# -> Int -> Int
naiveStrlen addr !n = case indexWord8OffAddr# addr 0# of
  0## -> n
  _ -> naiveStrlen (plusAddr# addr 1#) (n + 1)
