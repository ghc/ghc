{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module TH_nested_splice where

a = [| 5 |]

b = [| $(a) |]

c = [|| 5 ||]

d = [|| $$(c) ||]
