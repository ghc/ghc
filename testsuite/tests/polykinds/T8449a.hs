{-# LANGUAGE GADTs, TypeFamilies, DataKinds, PolyKinds #-}

module T8449a where

data Lst a = Nil | Cons a (Lst a)

data family SList (a :: k)
data instance SList (a :: Lst k) where
   SNil :: SList 'Nil
