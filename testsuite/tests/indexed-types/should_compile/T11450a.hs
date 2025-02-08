{-# LANGUAGE TypeFamilies #-}

module T11450a where

class C x where
  type T x

instance C (Either a b) where
  type T (Either _ b) = b -> b
