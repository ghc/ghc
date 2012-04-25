{-# LANGUAGE TemplateHaskell, DataKinds, PolyKinds #-}

module T6005 where

$( [d|
   data Nat = Zero | Succ Nat
   data Proxy a = Proxy
   foo :: Proxy 'Zero
   foo = foo
   |])
