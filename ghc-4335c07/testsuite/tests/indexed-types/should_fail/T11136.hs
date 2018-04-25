{-# LANGUAGE TypeFamilies #-}

module T11136 where

class C a where
  type D a
  type instance D a x = x
