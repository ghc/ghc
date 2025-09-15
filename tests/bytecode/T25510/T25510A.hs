{-# LANGUAGE TemplateHaskellQuotes #-}

module T25510A where

import Language.Haskell.TH

a :: Q Exp
a = [| 114514 |]
