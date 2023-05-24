{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Irr
where

import GHC.Exts hiding (List)

data List a = Nil | Cons !a !(List a)

length'' :: Bool -> List a -> Int#
length'' !trigger !xs = if trigger then countA 0# xs else countB 0# xs
  where countA !n Nil = n
        countA !n (Cons _ as) = countB (n +# 1#) as
        countB !n Nil = n
        countB !n (Cons _ as) = countA (n +# 2#) as
        {-# NOINLINE countA #-}
        {-# NOINLINE countB #-}
