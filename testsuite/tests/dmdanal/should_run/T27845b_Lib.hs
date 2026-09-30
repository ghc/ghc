module T27845b_Lib where

outer :: a -> Int -> [a]
outer a n =
  let {-# INLINABLE [2] g #-}
      g x = lazyUse a (x + 0) ++ lazyUse a (x + 1) ++ lazyUse a (x + 2)
  in g n ++ g (n + 1) ++ g (n + 2)
{-# NOINLINE outer #-}

{-# NOINLINE lazyUse #-}
lazyUse :: a -> Int -> [a]
lazyUse p x = p `seq` replicate x p

{-# RULES "lazyUse" [1] forall p x. lazyUse p x = [] #-}
