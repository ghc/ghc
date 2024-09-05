{-# LANGUAGE LinearTypes, TemplateHaskell #-}

module ModifiersUnexpectedInQuote where

import Language.Haskell.TH

x :: Q Exp
x = [| undefined :: a %1 %1 -> b |]
