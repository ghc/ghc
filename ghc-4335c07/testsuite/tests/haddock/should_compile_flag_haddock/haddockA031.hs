module ShouldCompile where

data A 
  = A 
  | {-| comment for B -} forall a. B a a 
  | forall a. Num a => C a {-^ comment for C -}
