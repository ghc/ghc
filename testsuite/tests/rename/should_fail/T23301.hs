{-# LANGUAGE DuplicateRecordFields, TypeFamilies #-}

module T23301 where

data family D a
data instance D Int  = MkD1 { fld :: Int } | MkD1b | MkD1c { fld :: Int, bar :: Bool }
data instance D Bool = MkD2 { fld :: Bool }

foo = fld
