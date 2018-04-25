{-# LANGUAGE TypeFamilies #-}
module TypeFamilyInstanceLHS where

type family F (a :: *) (b :: *) :: *
type instance F Int  _ = Int
type instance F Bool _ = Bool

foo :: F Int Char -> Int
foo = id
