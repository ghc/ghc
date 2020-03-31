{-# LANGUAGE ImplicitParams #-}
module T17930 where

foo :: (?b :: Bool, Show a) => a -> String
foo x | ?b        = show x ++ "!"
      | otherwise = show x ++ "."
{-# INLINABLE[0] foo #-}

str :: String
str = let ?b = True in foo "Hello"
