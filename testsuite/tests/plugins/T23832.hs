{-# OPTIONS_GHC -fplugin DefaultMultiParam #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

class C a b where
    op :: a -> b -> ()

instance C Double Int where
    op _ _ = ()

main :: IO ()
main = pure $ op 1 2
