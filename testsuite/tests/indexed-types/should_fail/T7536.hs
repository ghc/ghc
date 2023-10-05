{-# LANGUAGE TypeFamilies #-}

module T7536 where

type T v = Int

type family TF a :: *
type instance TF (T a) = a

