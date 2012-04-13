{-# LANGUAGE DataKinds, TypeOperators #-}

module T5948 where

type Foo = (Int ': '[])

type Bar = Int ': '[]
