{-# LANGUAGE TypeFamilies #-}
module T16002 where

data A
type family B (x :: *) :: * where
  A x = x
