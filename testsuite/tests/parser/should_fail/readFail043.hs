-- Check error message for groups if we don't have the right extension turned on

module Foo where

import Data.List
import GHC.Exts

foo = [ ()
      | x <- [1..10]
      , then group by x using groupWith
      , then group by x using groupWith
      , then group using inits
      ]

