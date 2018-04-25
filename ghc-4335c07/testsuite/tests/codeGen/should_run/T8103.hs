{-# LANGUAGE MagicHash #-}
module Main where
import T8103_A

float_text = case (0.0## `foo` 1.2##) of
               0.0## -> "1"
               _     -> "0"
main = putStrLn (float_text)
