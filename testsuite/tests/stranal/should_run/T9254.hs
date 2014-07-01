{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Main where
import GHC.Exts

f :: (() -> (# Int#, () #)) -> ()
{-# NOINLINE f #-}
-- Strictness signature was (7.8.2)
--    <C(S(LS)), 1*C1(U(A,1*U()))>
-- I.e. calls k, but discards first component of result
f k = case k () of (# _, r #) -> r

g :: Int -> ()
g y = f (\n -> (# case y of I# y2 -> h (h (h (h (h (h (h y2)))))), n #))
   -- RHS is big enough to force worker/wrapper

{-# NOINLINE h #-}
h :: Int# -> Int#
h n = n +# 1#

main = print (g 1)
