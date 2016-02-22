{-# LANGUAGE TemplateHaskell #-}
module QQ where

import Language.Haskell.TH.Quote
import Language.Haskell.TH

pq = QuasiQuoter { quoteDec = \_ -> [d| f x = x |],
                   quoteType = \_ -> [t| Int -> Int |],
                   quoteExp = \_ -> [| $(varE (mkName "x")) + 1::Int |],
                   quotePat = \_ -> [p| Just x |] }

