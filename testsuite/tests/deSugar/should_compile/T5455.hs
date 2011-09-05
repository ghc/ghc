{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
module T5455 where

-- We should get just one error message for each defn

w :: String -> String
w x = let (_:_) = x in "1"

w2 :: String -> String
w2 x2 = let (_a:_b) = x2 in "1"
