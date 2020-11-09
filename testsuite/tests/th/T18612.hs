{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module T18612 where

import Data.Functor.Identity
import Data.Proxy
import Language.Haskell.TH

f :: $(arrowT `appT` (conT ''Identity `appT` (tupleT 1 `appT` (tupleT 0)))
              `appT` (conT ''Identity `appT` (tupleT 1 `appT` (tupleT 0))))
f $(conP 'Identity [tupP [tupP []]]) = $(conE 'Identity `appE` tupE [tupE []])

type G = $(conT ''Proxy `appT` (promotedTupleT 1 `appT` (tupleT 0)))
