{-# LANGUAGE MagicHash #-}
-- !!! Negative unboxed literals, part 2

import GHC.Exts

main = do
  --Newly implemented: don't parse this as subtraction (Prelude.-):
  print (I# (negateInt# -3#))
  print (F# (negateFloat# -3.0#))
  print (D# (negateDouble# -3.0##))
  --nor this as  let (-) f 1# = ...
  print (let { f -1# = True } in f (-1#))

