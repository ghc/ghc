module A where

newtype A = A { unA :: Int }

showA :: A -> String
showA = show . unA
