{-# LANGUAGE TemplateHaskell #-}
module Foo where

import Language.Haskell.TH

x :: Int
x = let args = [| show $(varE (mkName "x")) |]
    in undefined
