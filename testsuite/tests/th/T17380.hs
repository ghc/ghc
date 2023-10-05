{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module T17380 where

import Data.Proxy
import Language.Haskell.TH

foo :: $(tupleT 1 `appT` (conT ''Maybe `appT` conT ''String))
foo = Just "wat"

bar :: Maybe String
bar = $(tupE [[| Just "wat" |]])

baz :: $(tupleT 1 `appT` (conT ''Maybe `appT` conT ''String)) -> Maybe String
baz (Just "wat") = Just "frerf"

quux :: Maybe String -> Maybe String
quux $(tupP [[p| Just "wat" |]]) = Just "frerf"

quuz :: Proxy $(promotedTupleT 1 `appT` (conT ''Maybe `appT` conT ''String))
quuz = Proxy :: Proxy (Maybe String)

fred :: Proxy (Maybe String)
fred = Proxy :: Proxy $(promotedTupleT 1 `appT` (conT ''Maybe `appT` conT ''String))
