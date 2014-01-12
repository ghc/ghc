{-# LANGUAGE RankNTypes, FlexibleInstances #-}

-- !!! Check that forall types can't be arguments
module ShouldFail where


data T s a = MkT s a
 
instance Ord a => Ord (forall s. T s a) 
-- A for-all should not appear as an argument to Ord



g :: T s (forall b.b)
g = error "urk"
