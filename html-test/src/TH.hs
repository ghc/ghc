{-# LANGUAGE TemplateHaskell #-}

module TH where

import Language.Haskell.TH

decl :: Q [Dec]
decl = [d| f x = x|]
