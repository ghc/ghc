-- Like T27628b, but the specialisation pattern is the *nullary*
-- constructor Tip (the call passes Tip; the box is also returned).
-- Reboxing a nullary constructor is free (it is a shared static
-- closure), so no warning should be emitted.
module T27628e where

data T = Tip | Bin Int T T

merge :: T -> T -> T
merge Tip t2 = t2
merge t1@(Bin k l r) t2 =
  case t2 of
    Tip         -> t1
    Bin _ l2 r2 -> Bin k (merge l l2) (merge r r2)

g :: T -> T
g t = merge t Tip
