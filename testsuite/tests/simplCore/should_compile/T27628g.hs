module T27628g where

data T a = Tip | Bin a (T a) (T a)

-- merge has no free value variables, so the float-out pass lifts it to
-- top level as poly_merge, abstracted over 'a'. The reboxing warning
-- should still point at merge's definition site.
f :: a -> T a -> T a
f x t = merge (Bin x Tip Tip) t
  where
    merge Tip t2 = t2
    merge t1@(Bin k l r) t2 =
      case t2 of
        Tip         -> t1
        Bin _ l2 r2 -> Bin k (merge l l2) (merge r r2)
