{-# LANGUAGE TemplateHaskell #-}

module T25197_TH where

import Language.Haskell.TH.Syntax

gen :: Int -> Q Exp
gen 0 = [| [] |]
gen n = [| $(lift (show n)) : $(gen (n-1)) |]
