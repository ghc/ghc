-- !!! Checking the exhaustiveness of constructor
-- !!! with labelled fields.
module ShouldCompile where

data E = B { a,b,c,d,e,f :: Bool }

bug x =
 case x of
  B _ _ _ _ True False    -> undefined
  B {e=True, f=False}     -> undefined
  B {a=a,f=False,e=False} -> undefined
