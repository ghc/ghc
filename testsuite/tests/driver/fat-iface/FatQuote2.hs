{-# LANGUAGE TemplateHaskellQuotes #-}
module FatQuote2 where

import FatQuote ()

import Language.Haskell.TH

a :: Q Exp
a = [| () |]


