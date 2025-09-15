{-# LANGUAGE TypeFamilies, PolyKinds #-}

module T10141 where

type family G (a :: k) where
   G Int  = Bool
   G Bool = Int
   G a    = a
