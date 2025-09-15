{-# LANGUAGE TypeFamilies, TypeOperators #-}

module T23664 where

class POrd a where
  type a >= b
  infix 4 >=
