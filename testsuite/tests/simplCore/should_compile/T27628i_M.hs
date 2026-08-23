-- Three INLINE functions with local loops named 'merge': f1 and f3 over
-- T, f2 over S. In an importing module all three loops arrive span-less
-- (iface unfoldings record no spans for local binders). f1's and f3's
-- copies render identically (same name and type) and must merge into
-- one warning; f2's differs in type and must stay separate.
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

-- Like f1 but recursing with the children swapped, so the two loops
-- stay distinct functions while their warnings render the same
f3 :: a -> T a -> T a
f3 x t = merge (Bin x Tip Tip) t
  where
    merge Tip t2 = t2
    merge t1@(Bin k l r) t2 =
      case t2 of
        Tip         -> t1
        Bin _ l2 r2 -> Bin k (merge r r2) (merge l l2)
{-# INLINE f3 #-}
