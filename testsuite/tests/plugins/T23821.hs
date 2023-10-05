{-# OPTIONS_GHC -fplugin DefaultInterference #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main where

class IsColor a where
    op :: a -> ()

instance IsColor (Int, Int, Int) where
    op _ = ()

main :: IO ()
main = pure $ op (1, 2, 3)
