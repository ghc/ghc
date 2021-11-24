{-# LANGUAGE OverloadedRecordDot #-}
module T20723 where

data Rec = Rec { as :: Int, dependency :: Int, signature :: Int, javascript :: Int }

res r = r.as + r.dependency + r.signature + r.javascript



