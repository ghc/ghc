{-# LANGUAGE TypeFamilies #-}
module T8034 where

class C a where
  type F a
  foo :: F a -> F a
