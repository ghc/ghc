{-# LANGUAGE PartialTypeSignatures #-}
module WildcardInInstanceHead where

class Foo k where
  bar :: k

instance Foo _ where
  bar = 3
