{-# OPTIONS_GHC -fglasgow-exts #-}

module ShouldSucceed3 where 


data T a where 
  C :: (T b) -> b -> T Int 
  D :: T Bool 

-- Tests scoped annotations 
foo :: T a -> a -> a 
foo (C (x::b)) (z::a) = z + 1 

