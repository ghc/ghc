{-# LANGUAGE TemplateHaskell #-}

module T8932 where

$([d|
   foo :: a -> a
   foo x = x
  |])

foo :: a
foo = undefined

