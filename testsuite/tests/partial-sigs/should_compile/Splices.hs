{-# LANGUAGE TemplateHaskell, NamedWildCards #-}

module Splices where

import Language.Haskell.TH
import Language.Haskell.TH.Lib (wildCardT)

metaType1 :: TypeQ
metaType1 = wildCardT

metaType2 :: TypeQ
metaType2 = [t| _ |]

metaType3 :: TypeQ
metaType3 = [t| _ -> _ -> _ |]

metaDec1 :: Q [Dec]
metaDec1 = [d| foo :: _ => _
               foo x y = x == y |]

metaDec2 :: Q [Dec]
metaDec2 = [d| bar :: _a -> _b -> (_a, _b)
               bar x y = (not x, y) |]

-- An expression with a partial type annotation
metaExp1 :: ExpQ
metaExp1 = [| Just True :: Maybe _ |]

metaExp2 :: ExpQ
metaExp2 = [| id :: _a -> _a |]
