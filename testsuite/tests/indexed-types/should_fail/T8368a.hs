{-# LANGUAGE TypeFamilies, GADTs #-}

module T8368a where

data family Fam a b
data instance Fam Int b where
  MkFam :: Fam Bool b
  