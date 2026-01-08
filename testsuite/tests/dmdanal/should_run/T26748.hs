{-# LANGUAGE Haskell98 #-}
module Main (main, x) where

data Eq a => D a = MkD { lazy_field :: a, strict_field :: !a }

x :: D ()
{-# INLINABLE x #-}
x = MkD { lazy_field = error "urk", strict_field = () }

main :: IO ()
main = print (strict_field x)
