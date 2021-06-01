{-# LANGUAGE UnicodeSyntax, LinearTypes #-}

module T19928 where

data R where
  D1 :: { d1 :: Int } %1 -> R
  Dp :: { dp :: Int } %p -> R
  Dl :: { dl :: Int } ⊸ R
