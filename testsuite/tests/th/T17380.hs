{-# LANGUAGE TemplateHaskell #-}
module T17380 where

import Language.Haskell.TH

foo :: $(tupleT 1 `appT` conT ''String)
foo = "wat"

bar :: String
bar = $(tupE [[| "wat" |]])

baz :: $(tupleT 1 `appT` conT ''String) -> String
baz "wat" = "frerf"

quux :: String -> String
quux $(tupP [[p| "wat" |]]) = "frerf"
