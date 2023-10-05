{-# LANGUAGE StandaloneDeriving #-} 
module Main where 
 
data A 
deriving instance Read A 
deriving instance Show A 
 
main = print (read "[]" :: [A])
-- Should successfully read the empty list

