{-# LANGUAGE RankNTypes #-}

module Church2 where 
-- Church numerals w/o extra type constructors: Should fail

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

-- should fail! 
exp2 :: CNat -> CNat -> CNat 
exp2 m n = n (mul m) n1 
