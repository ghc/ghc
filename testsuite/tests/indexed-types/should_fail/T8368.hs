{-# LANGUAGE TypeFamilies, GADTs #-}

module T8368 where

data Foo = Bar

data family Fam a
data instance Fam a where
  MkFam :: Foo
  