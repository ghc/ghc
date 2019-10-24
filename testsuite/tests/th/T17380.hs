{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module T17380 where

import Data.Proxy
import Language.Haskell.TH

foo :: $(tupleT 1 `appT` conT ''String)
foo = "wat"

bar :: String
bar = $(tupE [[| "wat" |]])

baz :: $(tupleT 1 `appT` conT ''String) -> String
baz "wat" = "frerf"

quux :: String -> String
quux $(tupP [[p| "wat" |]]) = "frerf"

quuz :: Proxy $(promotedTupleT 1 `appT` conT ''String)
quuz = Proxy :: Proxy String

fred :: Proxy String
fred = Proxy :: Proxy $(promotedTupleT 1 `appT` conT ''String)
