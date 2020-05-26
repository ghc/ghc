{-# LANGUAGE ImpredicativeTypes, RankNTypes #-}

module Church1 where
-- Church numerals w/o extra type constructors

type CNat = forall a. (a->a) -> a -> a

n0 :: CNat
n0 = \f z -> z

n1 :: CNat
n1 = \f z -> f z

nsucc :: CNat -> CNat
nsucc n = \f z -> f (n f z)

add, mul :: CNat -> CNat -> CNat
add m n = \f -> \a -> m f (n f a)
mul m n = \f -> \a -> m (n f) a

-- already works with GHC 6.4
exp0 :: CNat -> CNat -> CNat
exp0 m n = n m

exp1 :: CNat -> CNat -> CNat
exp1 m n = n (mul m) n1

foldNat :: CNat -> (a -> a) -> a -> a
foldNat n = n

idC :: CNat -> CNat
idC x = foldNat x nsucc n0

inc :: CNat -> CNat
inc x = foldNat x nsucc n1
