{-# LANGUAGE RebindableSyntax, TemplateHaskell #-}

import Prelude
import Language.Haskell.TH.Syntax

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse _ a b = a+b

x :: Int
x = $$( [|| if True then 10 else 15 ||] )

main :: IO ()
main = print x
