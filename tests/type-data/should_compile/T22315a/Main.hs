{-# LANGUAGE NoImplicitPrelude #-}
module T22315a.Main where

import T22315a.Lib

data Proxy (a :: TypeLevel)

bar :: Proxy Mk -> ()
bar = foo
