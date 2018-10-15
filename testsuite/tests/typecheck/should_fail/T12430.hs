{-# LANGUAGE TypeFamilyDependencies #-}
module T12430 where

type C a = Int
type family F x = y | y -> x where
  F x = C x
