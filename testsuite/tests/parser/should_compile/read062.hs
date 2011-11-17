{-# OPTIONS_GHC -XTransformListComp #-}

module Foo where

import Data.List
import GHC.Exts

foo = [ ()
      | x <- [1..10]
      , then take 5
      , then sortWith by x
      , then group by x using groupWith
      , then group using inits
      , then group by x using groupWith
      ]

