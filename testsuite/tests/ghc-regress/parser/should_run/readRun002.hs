{-# LANGUAGE MagicHash #-}
-- !!! Negative unboxed literals, part 1
-- They don't have to be as standards-compliant
-- or follow so many weird cases as the normal
-- boxed version.  In particular, normal unboxed
-- subtraction is -#, `minusFloat#`, -##, `minusInteger#`
-- and unboxed negation is negate{Int,Float,Double}#
-- .  (-) and negate are kind errors.  So we will
-- assume that we don't need to parse infix (-) nicely
-- when unboxed numbers are involved (even though someone
-- "could" hide the Prelude's version and define (-) themself).
-- Also we won't care here whether having a space (- 3#) works.

-- Make sure the parsing is actually the correct
-- one by running this after it's compiled.

import GHC.Exts

--is floating-point consistently safe to test like this,
--if we stick to integral values?
main = do
  --These work with any ghc
  print (I# (negateInt# (-3# -# -4#)))
  print (F# (negateFloat# (-3.0# `minusFloat#` -4.0#)))
  print (D# (negateDouble# (-3.0## -## -4.0##)))
  print (I# (-3# ^# 4#)) --different from (boxed) Haskell98 (-3 ^ 4)
  print ( case -1# of { -1# -> True } )
  print ( case 1# of { -1# -> True; _ -> False } )
  print ( case -0# of { 0# -> True } )

infixr 8  ^#  --just like ^, binds tighter than - (which is infixl 6)
( ^# ) :: Int# -> Int# -> Int#
base ^# 0# = 1#
base ^# exponent = base *# (base ^# ( exponent -# 1# ))

