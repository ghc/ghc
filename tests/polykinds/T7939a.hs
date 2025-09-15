{-# LANGUAGE TypeFamilies, PolyKinds #-}

module T7939a where

type family F a where
  F Int = Bool
  F Maybe = Char
