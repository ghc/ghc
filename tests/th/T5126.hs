{-# LANGUAGE TemplateHaskell #-}
module T5126 where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

f :: Q [Dec]
f =
    [d|
        x2 :: $(conT ''Int)
        x2 = undefined
    |]
