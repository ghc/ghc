{-# LANGUAGE MagicHash #-}

import GHC.Exts
import Control.Exception

f :: ArithException -> Int#
f x = raise# (toException x)

main = print (I# (f Overflow))
