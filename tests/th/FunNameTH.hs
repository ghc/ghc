{-# LANGUAGE TemplateHaskell #-}

module FunNameTH where

import Language.Haskell.TH

f1 :: forall a. $(conT (mkName "->")) [a] Bool
f1 = null

f2 :: forall a. $(conT ''(->)) [a] Bool
f2 = null