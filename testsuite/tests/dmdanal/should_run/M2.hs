module M2 where

-- f's stable unfolding template is the PAP `mk g`, which is not
-- eta-expanded, so its DmdType has depth 0. The big lambda keeps the
-- template large enough that f does not certainlyWillInline, so it is
-- worker/wrappered.
{-# INLINABLE [1] f #-}
f :: Int -> Int -> Float
f = mk (\y -> succ . succ . succ . succ . succ . succ . succ . succ . succ $ y)

-- Arity 1: the `let` does real work, so mk is not eta-expanded past it.
-- The returned function really forces x.
{-# NOINLINE mk #-}
mk :: (Int -> Int) -> Int -> Int -> Float
mk t = let s = t 1 in \dummy x -> x `seq` fromIntegral (t (dummy + s))

{-# INLINE mkFast #-}
mkFast :: (Int -> Int) -> Int -> Int -> Float
mkFast t = \dummy x -> fromIntegral (t dummy)

-- Active only before phase 1: rewrites f's RHS early, but is dead by
-- the time f's stable unfolding (activation [1]) is inlined at call
-- sites.
{-# RULES "mk" [~1] forall t. mk t = mkFast t #-}

{-# NOINLINE g #-}
g :: Int -> Int
g x = x + 1
