{-# LANGUAGE LinearTypes #-}
module T19120 where

notL :: Bool %1 -> Bool
notL True = False
notL False = True

z :: Bool %1 -> Bool
z x | notL x = True
z x | otherwise = notL x
