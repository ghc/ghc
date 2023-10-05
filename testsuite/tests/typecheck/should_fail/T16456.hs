{-# LANGUAGE PolyKinds #-}
module T16456 where

data T p = MkT

foo :: T Int
foo = _
