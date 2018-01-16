{-# LANGUAGE MagicHash, UnboxedTuples #-}

--module Foo where

import GHC.Exts

--you can use this if you want to test running it...
main = print (I# (
      f1pat 1# +# f1prepat 1#
   +# f2pat 1# +# f2prepat 1#
   +# f3pat 1# +# f3prepat 1#
  ))

--unboxed tuples are of sizes 1,2,3...
--(normal tuples are 0,2,3...)

--make sure it's really the _unboxed_ tuples
--being used by putting unboxed values in,
--which are forbidden in boxed tuples

f1 :: Int# -> (# Int# #)
f1 i = (# i #)
-- a space is needed in (# #) so that it's not
-- lexed/parsed as an operator named "##"
--(even though the error message about mismatched
--kinds for "instance Functor (# #)" names the type
--as "(##)"
--  Kind mis-match
--  Expected kind `* -> *', but `(##)' has kind `? -> (#)'
--  In the instance declaration for `Functor (##)'
f1prefix :: Int# -> (# #)
f1prefix i = (# #)
--test that prefix and non-prefix versions
--are the same type by switching the case-argument
f1pat a = case f1prefix a of (# #) -> 1#
f1prepat a = case f1 a of (# i #) -> i +# 1#

f2 :: Int# -> (# Int#, Int# #)
f2 i = (# i, i #)
f2prefix :: Int# -> (#,#) Int# Int#
f2prefix i = (#,#) i i
f2pat a = case f2prefix a of (# i, j #) -> i +# j
f2prepat a = case f2 a of (#,#) i j -> i +# j

f3 :: Int# -> (# Int#, Int#, Int# #)
f3 i = (# i, i, i #)
f3prefix :: Int# -> (#,,#) Int# Int# Int#
f3prefix i = (#,,#) i i i
f3pat a = case f3prefix a of (# i, j, k #) -> i +# j +# k
f3prepat a = case f3 a of (#,,#) i j k -> i +# j +# k

