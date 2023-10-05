{-# LANGUAGE DataKinds #-}

module T19843m where

data Foo = LongName
data FongName
wrongName = wrongName

type Bar = WrongName
