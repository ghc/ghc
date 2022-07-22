{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
module Main where

import Language.Haskell.TH.Syntax

p :: Bool
p = $(const [| True |] ('a' + 'a'))

q :: Bool
q = $$(const [|| True ||] ('a' + 'a'))

main = print (p, q)
