{-# OPTIONS_GHC -fplugin DefaultInvalid #-}
module Main where

class C a where
    op :: a -> ()

instance C Double where
    op x = ()

bar :: a -> ()
bar = op

main :: IO ()
main = pure ()
