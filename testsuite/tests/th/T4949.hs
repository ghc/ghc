{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Foo where

import Language.Haskell.TH

x :: Int
x = let args = [| show $(varE (mkName "x")) |]
    in undefined
