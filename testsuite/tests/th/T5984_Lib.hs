{-# LANGUAGE TemplateHaskell #-}

module T5984_Lib where

import Language.Haskell.TH

nt :: Q [Dec]
nt = return [NewtypeD [] foo [] Nothing
             (NormalC foo [(NotStrict, ConT ''Int)]) []]
  where foo = mkName "Foo"

dt :: Q [Dec]
dt = return [DataD [] bar [] Nothing [NormalC bar [(NotStrict, ConT ''Int)]] []]
  where bar = mkName "Bar"
