{-# LANGUAGE TemplateHaskell #-}
module Main where

$([d| instance Show (a -> b) where
         showsPrec _ _ = showString "<function>"
  |])

main = print id