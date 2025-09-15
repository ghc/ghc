{-# LANGUAGE GADTs #-}

module ShouldSucceed3 where 


data T a where 
  C :: T b -> b -> T Int 
  D :: T Bool 

-- Tests scoped annotations 
foo :: T a -> a -> a 
foo (C y (x::b)) (z::a) = z + 1 

