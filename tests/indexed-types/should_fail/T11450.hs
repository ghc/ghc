{-# LANGUAGE TypeFamilies #-}

module T11450 where

class C x where
  type T x

instance C (Either a b) where
  type T (Either b a) = b -> a
