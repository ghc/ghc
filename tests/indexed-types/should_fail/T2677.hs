{-# LANGUAGE TypeFamilies #-}

module T2677 where

type family A x
type instance A a = Bool
type instance A Int = Char
