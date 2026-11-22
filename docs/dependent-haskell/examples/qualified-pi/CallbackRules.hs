{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module CallbackRules (Choice, makeChoice, readKept) where

-- | Store an integer at the result index of a callback.
data Choice (b :: Bool) = Choice Int

-- | Use the callback only when the integer is negative.
{-# NOINLINE makeChoice #-}
makeChoice :: foreach (f :: Bool -> Bool) -> foreach (x :: Bool) -> Int -> Choice (f x)
makeChoice f x value = Choice
    (if value < 0 then if f x then 1 else 0 else value)

-- | Read the payload at any index.
readChoice :: Choice b -> Int
readChoice (Choice value) = value

-- | Read the payload at the index of the supplied callback and input.
{-# NOINLINE readKept #-}
readKept :: foreach (f :: Bool -> Bool) -> foreach (x :: Bool) -> Choice (f x) -> Int
readKept _ _ = readChoice

{-# RULES "read/callback" [1] forall f x payload. readKept f x payload = readChoice payload #-}
