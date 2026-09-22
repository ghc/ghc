{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fno-worker-wrapper -fno-cpr-anal #-}
module T27842A (fun, just_con, gt_con) where

-- The native EPT run gives fun the TagSig TagFun[TagTuple[TagEPT, TagDunno]]:
fun :: Int -> (# [Int], Int #)
fun x = let xs = x : xs in (# xs, x #)
{-# NOINLINE fun #-}

-- In native code a static constructor, in bytecode an unevaluated closure.
just_con :: Maybe Int
just_con = Just 42
{-# NOINLINE just_con #-}

-- The same for a nullary constructor. Without its pointer tag, the object code
-- in T27842N would take the LT alternative, so GT shows the difference.
gt_con :: Ordering
gt_con = GT
{-# NOINLINE gt_con #-}
