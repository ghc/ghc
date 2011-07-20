
{-# OPTIONS_GHC -XLiberalTypeSynonyms #-}

module ShouldCompile where

type T a b = a
type S m   = m ()

f :: S (T Int)
f = undefined

