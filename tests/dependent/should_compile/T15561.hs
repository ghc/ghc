{-# LANGUAGE TypeFamilies, DataKinds, GADTs #-}

module T15561 where

class HasIndex a where
    type Index a
    emptyIndex :: IndexWrapper a
instance HasIndex [a] where
    type Index [a] = Int
    emptyIndex = Wrap 0

type family UnwrapAnyWrapperLikeThing (a :: t) :: k

data IndexWrapper a where
    Wrap :: Index a -> IndexWrapper a

type instance UnwrapAnyWrapperLikeThing ('Wrap a :: IndexWrapper [b]) = a

