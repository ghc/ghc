{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Irr3  -- like Irr, but simplified
where

import GHC.Exts hiding (List)

type List :: TYPE UnliftedRep
data List = Nil | Cons !List

length'' :: Int# -> List -> Int#
length'' trigger xs =
    case trigger of 0# -> countA 0# xs
                    _  -> countB 0# xs
  where countA n Nil = n
        countA n (Cons as) = countB (n +# 1#) as
        countB n Nil = n
        countB n (Cons as) = countA (n +# 2#) as
        {-# NOINLINE countA #-}
        {-# NOINLINE countB #-}
