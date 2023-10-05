{-# LANGUAGE TemplateHaskellQuotes #-}
module FatQuote where

import Language.Haskell.TH

a :: Q Exp
a = [| () |]


