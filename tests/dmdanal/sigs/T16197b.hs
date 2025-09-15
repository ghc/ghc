-- | The same as T16197, but a bit more distilled.
-- Important takeaway: The signature of `f` may not say "strict in the Bool
-- field of T", otherwise the Simplifier will drop the `seq` on the `Bool` at
-- call sites after unboxing the `T`.
--
-- Now (with #21497) this has  a twist. When we do w/w we insert seqs to make the worker strict in
-- strict fields. This means the simplifier might drop the seq at the call site
-- but we will seq inside the worker so things still work.
-- So instead of checking the strictness sig we now check if there is a seq in the worker.
module T16197b where

data T = T !Bool
data Box a = Box a

f :: T -> Box Bool
f (T b) = Box b
{-# NOINLINE f #-} -- I like NOINLINE better than artificial recursion, YMMV
