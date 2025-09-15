{-# LANGUAGE TypeFamilies #-}

module T8018 where

data Foo a = Bar (F a)

type family F a where
  F (Foo a) = Bool