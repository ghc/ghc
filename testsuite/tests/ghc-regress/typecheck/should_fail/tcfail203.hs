-- trac #2806

{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}

module Foo where

import GHC.Base

pass1 = 'a'
    where !x = 5#

pass2 = 'a'
    where !(I# x) = 5

pass3 = 'a'
    where !(b, I# x) = (True, 5)

pass4 = 'a'
    where !(# b, I# x #) = (# True, 5 #)

pass5 = 'a'
    where !(# b, x #) = (# True, 5# #)

fail1 = 'a'
    where x = 5#

fail2 = 'a'
    where (I# x) = 5

fail3 = 'a'
    where (b, I# x) = (True, 5)

fail4 = 'a'
    where (# b, I# x #) = (# True, 5 #)

fail5 = 'a'
    where (# b, x #) = (# True, 5# #)

fail6 = 'a'
    where (I# !x) = 5

fail7 = 'a'
    where (b, !(I# x)) = (True, 5)

fail8 = 'a'
    where (# b, !(I# x) #) = (# True, 5 #)

fail9 = 'a'
    where (# b, !x #) = (# True, 5# #)
{-
-- Now in tcfail203a.hs, because it's an error
fail10 = 'a'
    where !(b, ~(c, (I# x))) = (True, (False, 5))
-}
