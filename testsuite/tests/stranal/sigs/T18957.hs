{-# OPTIONS_GHC -O2 -fforce-recomp #-}
{-# LANGUAGE BangPatterns #-}

-- | This ticket is about the demand `seq` puts its first argument under and how
-- that affects call demands.
module T18957 where

-- | Should put its first argument under head demand
-- Note that seq' is like seq, but NOINLINE, so the calling code will not have
-- access to the case binder. That is the difference between 'h1' and 'h2'.
seq' :: a -> b -> b
seq' a b = seq a b
{-# NOINLINE seq' #-}

-- | The first argument is evaluated at once, but called every time it's
-- evaluated
g :: (Int -> Int) -> Int -> Int
g f x = if x < 100 then f x else 200

-- | The first argument is evaluated multiple times, but called at most once
-- every time it's evaluated
h1 :: (Int -> Int) -> Int -> Int
h1 f x = f `seq'` if x < 100 then f x else 200

-- | Like h1, but using `seq` directly, which will rewrite the call site
-- of @f@ to use the case binder instead, which means we won't evaluate it an
-- additional time. So evaluated once and called once.
h2 :: (Int -> Int) -> Int -> Int
h2 f x = f `seq` if x < 100 then f x else 200

h3 :: (Int -> Int) -> Int -> Int
h3 f x = if x < 100 then f x + f (x+1) else 200
