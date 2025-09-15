{-# LANGUAGE TemplateHaskell #-}
module Types where

import qualified Hospital
import Language.Haskell.TH

genCode :: Q [Dec]
genCode =
    let s = Hospital.foo 5
    in [d|string = s|]
