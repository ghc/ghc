module ShouldCompile where

data A 
  -- | A comment that documents the first constructor
  = A 
  -- | comment for B 
  | {-^ comment for A -} B 
  -- | comment for C  
  | C 
  | D
