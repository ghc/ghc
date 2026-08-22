-- The INLINE unfolding of f carries the local loop 'merge' into importing
-- modules, where it is specialised without a source span: iface unfoldings
-- record no spans for local binders.
module T27628h_M where

data T a = Tip | Bin a (T a) (T a)

f :: a -> T a -> T a
f x t = merge (Bin x Tip Tip) t
  where
    merge Tip t2 = t2
    merge t1@(Bin k l r) t2 =
      case t2 of
        Tip         -> t1
        Bin _ l2 r2 -> Bin k (merge l l2) (merge r r2)
{-# INLINE f #-}
