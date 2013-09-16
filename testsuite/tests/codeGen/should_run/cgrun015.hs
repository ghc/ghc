{-# LANGUAGE MagicHash #-}
module Main ( main ) where

import Foreign
import Foreign.C
import GHC.Exts

data CList = CNil | CCons Int# CList

mk :: Int# -> CList
mk n  = if isTrue# (n ==# 0#)
	then CNil
	else CCons 1# (mk (n -# 1#))

clen :: CList -> Int#
clen CNil = 0#
clen (CCons _ cl) = 1# +# (clen cl)

main = case (clen list4) of
		len4 ->
		  case (len4 +# len4) of
		    8# -> finish 65#	-- 'A'
		    _  -> finish 66#	-- 'B'
      where
      list4	= mk 4#

finish :: Int# -> IO ()
finish n = c_putchar (castCharToCChar (C# (chr# n))) >> return ()

foreign import ccall unsafe "putchar"
  c_putchar :: CChar -> IO CInt
