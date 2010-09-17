{-# LANGUAGE ImplicitParams #-}

module IPFail where

f0 :: (?x :: Int) => () -> Bool
f0 () = let ?x = 5 in ?x
