{-# LANGUAGE TypeFamilies #-}

module ShouldCompile where

data X1 = X1

class C t where
    type D t
    f :: t -> D t -> ()

instance C X1 where
    type D X1 = Bool -> Bool
    f _ h = ()

foo = f X1 (\x -> x)
