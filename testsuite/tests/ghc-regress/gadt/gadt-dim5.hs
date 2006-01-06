{-# OPTIONS_GHC -fglasgow-exts #-}

module ShouldSucceed2 where 

data T a where 
  C :: Int -> T Int 
  D :: Bool -> T Bool 


foo :: T a -> a 
foo (C x) = x + 1 
foo (D x) = True 

