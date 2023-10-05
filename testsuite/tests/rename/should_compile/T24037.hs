{-# LANGUAGE TypeFamilies, TypeOperators #-}

module T24037 where

class POrd a where
  type Geq a b
  infixr 6 `Geq`
