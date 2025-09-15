{-# LANGUAGE TemplateHaskellQuotes #-}
module FatQuote1 where

import FatQuote ()

import Language.Haskell.TH

a :: Q Exp
a = [| () |]


