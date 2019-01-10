module T16059e where

import T16059b

type Const a b = a

-- This is accepted, since the rank-n type `Foo` never makes it into the type
-- of `f` post-expansion...
f :: Const Int Foo -> Int
f _ = 42

-- ...but this *is* rejected, since `Foo` does make it into the type of `g`
-- post-expansion.
g :: Const Foo Foo -> Int
g _ = f 27
