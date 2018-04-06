{-# LANGUAGE RankNTypes #-}

module Memo where
import Data.Bits

type Memo a = forall r. (a -> r) -> (a -> r)


memo2 :: Memo a -> Memo b -> (a -> b -> r) -> (a -> b -> r)
memo2 a b = a . (b .)

wrap :: (a -> b) -> (b -> a) -> Memo a -> Memo b
wrap i j m f = m (f . i) . j


pair :: Memo a -> Memo b -> Memo (a,b)
pair m m' f = uncurry (m (\x -> m' (\y -> f (x,y))))


bits :: (Num a, Ord a, Bits a) => Memo a
bits f = apply (fmap f identity)

data IntTrie a = IntTrie (BitTrie a) a (BitTrie a)  -- negative, 0, positive
data BitTrie a = BitTrie a (BitTrie a) (BitTrie a)


instance Functor BitTrie where
    fmap f ~(BitTrie x l r) = BitTrie (f x) (fmap f l) (fmap f r)



instance Functor IntTrie where
    fmap f ~(IntTrie neg z pos) = IntTrie (fmap f neg) (f z) (fmap f pos)

-- | Apply the trie to an argument.  This is the semantic map.
apply :: (Ord b, Num b, Bits b) => IntTrie a -> b -> a
apply (IntTrie neg z pos) x =
    case compare x 0 of
        LT -> applyPositive neg (-x)
        EQ -> z
        GT -> applyPositive pos x

applyPositive :: (Num b, Bits b) => BitTrie a -> b -> a
applyPositive (BitTrie one eve od) x
    | x == 1 = one
    | testBit x 0 = applyPositive od (x `shiftR` 1)
    | otherwise   = applyPositive eve (x `shiftR` 1)

identity :: (Num a, Bits a) => IntTrie a
identity = IntTrie (fmap negate identityPositive) 0 identityPositive



identityPositive :: (Num a, Bits a) => BitTrie a
identityPositive = go
    where
      go = BitTrie 1 (fmap (`shiftL` 1) go) (fmap (\n -> (n `shiftL` 1) .|. 1) go)
