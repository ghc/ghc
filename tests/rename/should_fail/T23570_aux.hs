{-# LANGUAGE TypeFamilies #-}

module T23570_aux where

class C a where
  type T a
  meth :: a -> T a
