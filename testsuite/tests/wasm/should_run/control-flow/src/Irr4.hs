{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Irr4  -- like Irr3, but with lifted types
where

import GHC.Exts hiding (List)

type List :: TYPE UnliftedRep
data List = Nil | Cons !List

length'' :: Int# -> List -> Int
length'' trigger xs =
    case trigger of 0# -> countA 0 xs
                    _  -> countB 0 xs
  where countA n Nil = n + I# trigger
        countA n (Cons as) = countB (n + 1) as
        countB n Nil = n + I# trigger
        countB n (Cons as) = countA (n + 2) as
        {-# NOINLINE countA #-}
        {-# NOINLINE countB #-}
