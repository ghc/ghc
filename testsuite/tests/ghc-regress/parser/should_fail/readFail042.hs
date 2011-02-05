-- Check error message for transforms if we don't have the right extension turned on

module Foo where

import Data.List
import GHC.Exts

foo = [ ()
      | x <- [1..10]
      , then take 5
      , then sortWith by x
      ]