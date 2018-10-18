{-# LANGUAGE TemplateHaskell #-}
module T12387 where

import Language.Haskell.TH.Lib

data Foo = Foo

$(do d <- instanceD (cxt []) (conT ''Eq `appT` conT ''Foo)
            [funD 'compare [clause [] (normalB $ varE 'undefined) []]]
     return [d])
