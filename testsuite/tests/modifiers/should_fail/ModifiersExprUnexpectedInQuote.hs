{-# LANGUAGE LinearTypes, TemplateHaskell, RequiredTypeArguments #-}

module ModifiersExprUnexpectedInQuote where

import Language.Haskell.TH

x :: Q Exp
x = [| undefined (Int %1 %1 -> Int) |]
