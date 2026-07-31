{-# LANGUAGE RebindableSyntax, TemplateHaskell #-}
module T18102b_aux where

import Prelude
import Language.Haskell.TH.Syntax

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse _ a b = a+b

intQuote :: Code Q Int
intQuote = [|| if True then 10 else 15 ||]
