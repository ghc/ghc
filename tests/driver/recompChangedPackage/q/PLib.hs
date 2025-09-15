{-# LANGUAGE FlexibleInstances #-}
module PLib where

p = 'e'

instance {-# OVERLAPPING #-} Show [Char] where
  show _ = "empty"
