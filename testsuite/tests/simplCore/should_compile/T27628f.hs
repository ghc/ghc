-- Like T27628b, but the specialised function is a *local* worker.
-- The warning should attribute it to the enclosing top-level binder f.
-- (merge captures n so that it is not floated to the top level.)
module T27628f where

data T = Tip | Bin Int T T

f :: Int -> T -> T
f n t0 = merge (Bin n Tip Tip) t0
  where
    merge :: T -> T -> T
    merge Tip _ = Tip
    merge t1@(Bin k l r) t2 =
      case t2 of
        Tip         -> t1
        Bin _ l2 r2 -> Bin (k + n) (merge l l2) (merge r r2)
