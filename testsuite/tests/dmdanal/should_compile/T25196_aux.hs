{-# LANGUAGE TemplateHaskell #-}

module T25196_aux where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

gen :: Int -> Q Exp
gen n = lamE (replicate n (newName "x" >>= varP)) [| () |]
