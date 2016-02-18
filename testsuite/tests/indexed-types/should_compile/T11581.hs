{-# LANGUAGE TypeFamilies #-}

module T11581 where

type family F a :: * -> *
type family G a

type instance G [a] = F a (Int,Bool)
