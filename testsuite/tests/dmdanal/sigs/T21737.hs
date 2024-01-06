{-# OPTIONS_GHC -fmax-worker-args=4 #-}

{-# LANGUAGE MagicHash, UnboxedTuples #-}

-- See Note [Worker argument budget]
module T21737 where

data T = MkT (# Int, Int, Int, Int #)

-- NB: -fmax-worker-args=4 at the top of this file!
-- We should unbox through the unboxed pair but not T
{-# NOINLINE f #-}
f :: Int -> (# Int, Int #) -> T -> Int
f x (# y, z #) (MkT (# x1, x2, x3, x4 #)) = x + y + z + x1 + x2 + x3 + x4

-- NB: -fmax-worker-args=4 at the top of this file!
-- Do split the triple *even if* that gets us to 6 args,
-- because the triple will take 3 registers anyway (not 1)
-- and we get to unbox a b c.
yes :: (# Int, Int, Int #) -> Int -> Int -> Int -> Int
yes (# a, b, c #) d e f = a + b + c + d + e + f
{-# NOINLINE yes #-}

data U = MkU (# Int, Int, Int, Int, Int, Int #)

-- NB: -fmax-worker-args=4 at the top of this file!
-- Don't unbox U, because then we'll pass an unboxed 6-tuple, all in registers.
no :: U -> Int
no (MkU (# a, b, c, d, e, f #)) = a + b + c + d + e + f
{-# NOINLINE no #-}

-- NB: -fmax-worker-args=4 at the top of this file!
-- Hence do not unbox the nested triple.
boxed :: (Int, Int) -> (Int, (Int, Int, Int)) -> Int
boxed (a,b) (c, (d,e,f)) = a + b + c + d + e + f
{-# NOINLINE boxed #-}

-- NB: -fmax-worker-args=4 at the top of this file!
-- Do split the inner unboxed triple *even if* that gets us to 5 args, because
-- the function will take 5 args anyway. But don't split the pair!
unboxed :: (Int, Int) -> (# Int, (# Int, Int, Int #) #) -> Int
unboxed (a,b) (# c, (# d, e, f #) #) = a + b + c + d + e + f
{-# NOINLINE unboxed #-}

-- Point: Demand on `x` is lazy and thus Unboxed
app :: ((# Int, Int #) -> (# Int, Int #)) -> (# Int, Int #) -> (# Int, Int #)
app g x = g x
