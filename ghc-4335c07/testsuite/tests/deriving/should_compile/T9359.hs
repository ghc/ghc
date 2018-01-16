{-# Language GADTs, PolyKinds, TypeFamilies, DataKinds #-}
module Fam where

data Cmp a where
 Sup ::      Cmp a
 V   :: a -> Cmp a
 deriving (Show, Eq)

data family   CmpInterval (a :: Cmp k) (b :: Cmp k) :: *
data instance CmpInterval (V c)         Sup          = Starting c
  deriving( Show )

