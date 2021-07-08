-- | The same as T16197, but a bit more distilled.
-- Important takeaway: The signature of `f` may not say "strict in the Bool
-- field of T", otherwise the Simplifier will drop the `seq` on the `Bool` at
-- call sites after unboxing the `T`.
module T16197b where

data T = T !Bool
data Box a = Box a

f :: T -> Box Bool
f (T b) = Box b
{-# NOINLINE f #-} -- I like NOINLINE better than artificial recursion, YMMV
