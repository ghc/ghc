{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
module T5455 where

-- No error message for this one: 
-- the pattern will never be demanded

w :: String -> String
w x = let (_:_) = x in "1"

-- We should get just one error message here

w2 :: String -> String
w2 x2 = let (a:as) = x2 in (a:a:as)
