module T23083 where

-- Just ($), but NOINLINE so that we don't inline it eagerly, subverting the
-- test case
($$) :: (a -> b) -> a -> b
($$) f x = f x
{-# NOINLINE ($$) #-}

g :: ((Integer -> Integer) -> Integer) -> (Integer -> Integer) -> Integer
g f h = f (h `seq` (h $$))
