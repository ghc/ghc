{-# LANGUAGE TemplateHaskell, ExplicitNamespaces #-}
module T14032a where

$([d|
  infix 4 type :*:
  infix 4 data :*:
  data a :*: b = a :*: b
  |])
