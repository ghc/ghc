{-# LANGUAGE GADTs #-}
module FloatEq where 


data T a where 
  T1 :: T Int
  T2 :: T a
  
  
h :: T a -> a -> Int
h = undefined


f x y = case x of 
          T1 -> y::Int
          T2 -> h x y

