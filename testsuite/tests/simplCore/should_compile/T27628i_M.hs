-- Two INLINE functions, each with a local loop named 'merge', over
-- different types. In an importing module both loops arrive span-less
-- (iface unfoldings record no spans for local binders) and render
-- identically, so their warnings must merge into one.
module T27628i_M where

data T a = Tip | Bin a (T a) (T a)
data S a = Leaf | Node a (S a) (S a)

f1 :: a -> T a -> T a
f1 x t = merge (Bin x Tip Tip) t
  where
    merge Tip t2 = t2
    merge t1@(Bin k l r) t2 =
      case t2 of
        Tip         -> t1
        Bin _ l2 r2 -> Bin k (merge l l2) (merge r r2)
{-# INLINE f1 #-}

f2 :: a -> S a -> S a
f2 x t = merge (Node x Leaf Leaf) t
  where
    merge Leaf t2 = t2
    merge t1@(Node k l r) t2 =
      case t2 of
        Leaf         -> t1
        Node _ l2 r2 -> Node k (merge l l2) (merge r r2)
{-# INLINE f2 #-}
