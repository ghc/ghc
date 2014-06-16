{-# LANGUAGE  MagicHash, BangPatterns #-}
module ShouldCompile where

import GHC.Exts

data CList = CNil | CCons Int# CList

mk :: Int# -> CList
mk n = case isTrue# (n ==# 0#) of
       False -> CNil
       _     -> CCons 1# (mk (n -# 1#))

clen :: CList -> Int#
clen CNil = 0#
clen (CCons _ cl) = 1# +# (clen cl)

main = putStr (case len4_twice of
	    8# -> "bingo\n"
	    _  -> "oops\n")
  where
    list4	= mk 4#
    !len4	= clen list4
    !len4_twice	= len4 +# len4
