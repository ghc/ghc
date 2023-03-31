{-# LANGUAGE TypeFamilies, FunctionalDependencies, RankNTypes, MultiParamTypeClasses #-}
module T4254b where

class FD a b | a -> b where
  op :: a -> b;
  op = undefined

instance FD Int Bool

fails :: forall a b. (a~Int,FD a b) => a -> Bool
fails  = op
-- Could fail: no proof that b~Bool
-- But can also succeed; it's not a *wanted* constraint

{- Interestingly, the ambiguity check for the type sig succeeds:

[G] FD Int b
[W] FD Int beta

We get [W] beta~b; we unify immediately, and then solve.
All before we interact the [W] FD Int beta with the
top-level instances (which would give rise to [W] beta~Bool).

One the other hand, from `fails = op` we get

[G] FD Int b
[W] FD Int Bool

Interacting those two gives [W] b~Bool; bu this doesn't
happen becase we now solve first.

-}