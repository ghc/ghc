{-# LANGUAGE TemplateHaskell #-}
module T15437 where

import T15437A

f :: Int
f = $$(foo)
