module SPEC where

class (Show a, Enum a) => MyShow a where
    myShow :: a -> String

instance MyShow Int where
    myShow = myShow_impl . succ

foo :: Int -> String
foo = myShow_impl

-- This pragma should not be necessary
-- {-# specialize myShow_impl :: Int -> String #-}

{-# INLINEABLE myShow_impl #-}
myShow_impl :: MyShow a => a -> String
-- Make it large enough not to inline
myShow_impl x = show . succ . succ . succ . succ . succ . succ .
                succ . succ . succ . succ . succ . succ . succ .
                succ . succ . succ $ x
