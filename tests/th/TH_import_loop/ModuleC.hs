
module ModuleC where

import Language.Haskell.TH

import {-# SOURCE #-} ModuleA

nothing = return [] :: Q [Dec]

