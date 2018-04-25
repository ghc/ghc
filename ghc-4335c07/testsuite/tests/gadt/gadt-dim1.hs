{-# LANGUAGE GADTs #-}

module ShouldCompile where

data T a where 
  C :: Int -> T Int 
  D :: Bool -> T Bool 

foo :: T a -> a 
foo (C x) = x 
foo (D x) = x 
