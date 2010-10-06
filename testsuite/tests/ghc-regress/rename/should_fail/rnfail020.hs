{-# LANGUAGE ScopedTypeVariables #-}

-- !!! Error messages with scoped type variables

module Foo where

data Set a = Set a

unionSetB :: Eq a => Set a -> Set a -> Set a
unionSetB (s1 :: Set a) s2 = unionSets s1 s2
 where
   unionSets :: Eq a => Set a -> Set a -> Set a
   unionSets a b = a


{- In GHC 4.04 this gave the terrible message:

    None of the type variable(s) in the constraint `Eq a'
	appears in the type `Set a -> Set a -> Set a'
    In the type signature for `unionSets'
-}
