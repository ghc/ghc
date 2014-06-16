{-# OPTIONS -fglasgow-exts -O -dshow-passes #-}

module Foo where
import GHC.Base

foo :: Int -> Int
foo (I# n#) = bar i i
   where i# = n# +# 1#
         i  = I# i#

bar :: Int -> Int -> Int
{-# INLINE [0] bar #-}
bar _ n = n

{-   The trouble here was 

    *** Simplify:
         Result size = 25
         Result size = 25
         Result size = 25
         Result size = 25
         Result size = 25
    *** Simplify:
         Result size = 25
         Result size = 25
         Result size = 25
         Result size = 25
         Result size = 25


    etc.  

The reason was this:
	x = n# +# 1#
 	i = I# x

Being an unboxed value, we were treating the argument context of x
as intersting, and hence inlining x in the arg of I#. But then we just
float it out again, giving an infinite loop.
-}
