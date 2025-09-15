module Bug where

data Foo a x = Foo x

refoo :: Foo a x -> Foo b x
{-# NOINLINE refoo #-}
refoo (Foo x) = Foo x

{-# RULES

"refoo/refoo" forall s.
  refoo (refoo s) = s  #-}
