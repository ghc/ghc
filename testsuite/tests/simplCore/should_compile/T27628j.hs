-- A mutually recursive pair in the style of T27628b. g calls both
-- functions with a constructor argument: the non-loop-breaker inlines
-- into g, but the loop breaker's call survives to SpecConstr, whose
-- warning classifies it as mutually recursive and names the sibling.
module T27628j where

data T = Tip | Bin Int T T

mergeA :: T -> T -> T
mergeA Tip t2 = t2
mergeA t1@(Bin k l r) t2 =
  case t2 of
    Tip         -> t1
    Bin _ l2 r2 -> Bin k (mergeB l l2) (mergeB r r2)

mergeB :: T -> T -> T
mergeB Tip t2 = t2
mergeB t1@(Bin k l r) t2 =
  case t2 of
    Tip         -> t1
    Bin _ l2 r2 -> Bin k (mergeA r r2) (mergeA l l2)

g :: Int -> T -> T
g x t = mergeA (Bin x Tip Tip) (mergeB (Bin x Tip Tip) t)
