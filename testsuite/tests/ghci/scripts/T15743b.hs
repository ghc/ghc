{-# LANGUAGE PolyKinds, TypeFamilies #-}

module T15743b where

class C (a :: k) where
  type F a (b :: k2)
