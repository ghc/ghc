{-# LANGUAGE StandaloneDeriving #-} 
module Main where 
 
data A 
deriving instance Read A 
 
main = seq (read "" :: A) (return ()) 
