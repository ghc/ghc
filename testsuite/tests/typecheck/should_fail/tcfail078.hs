module ShouldFail where

-- !!! Using a type constructor as a class name

f :: Integer i => i 
f =               0    
