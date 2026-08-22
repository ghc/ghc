-- A return-argument-unchanged function in the style of IntSet.difference
-- (containers#1242): the 'Tip -> t1' branch returns the box, so the
-- specialisation on Bin reboxes and loses sharing.
-- Expect a -Wspec-constr-reboxing warning for 'merge'.
module T27628b where

data T = Tip | Bin Int T T

merge :: T -> T -> T
merge Tip _ = Tip
merge t1@(Bin k l r) t2 =
  case t2 of
    Tip         -> t1
    Bin _ l2 r2 -> Bin k (merge l l2) (merge r r2)

f :: Int -> T -> T
f n t = merge (Bin n Tip Tip) t
