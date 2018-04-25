module C where

import Language.Haskell.TH

import {-# SOURCE #-} A

nothing = return [] :: Q [Dec]
