{-# LANGUAGE MagicHash #-}
module Main where
import T8103_A
import GHC.Exts (isTrue#, (==##))

float_text = if isTrue# ((0.0## `foo` 1.2##) ==## 0.0##)
             then "1"
             else "0"
main = putStrLn (float_text)
