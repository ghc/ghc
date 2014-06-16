{-# LANGUAGE TemplateHaskell #-}
module TH_NestedSplices where

import Language.Haskell.TH

import TH_NestedSplices_Lib 
-- This import brings in
--   spliceExpr :: String -> Q Exp -> Q Exp
--   declareFun :: String -> Q [Dec]

-- Top level splice without $
declareFun "a" 

-- Splice inside splice
$(declareFun $(stringE "b"))

-- Splice inside splice without outer $
declareFun $(stringE "c")

-- Ordinary splicing
f x = $(spliceExpr "boo" [| x |])

-- Splice inside splice
g x = $(spliceExpr $(litE (stringL "boo")) [| x |])

-- Ordinary splice inside bracket
h1 = [| $(litE (integerL 3)) |]

-- Splice inside splice inside bracket
h2 = [| $(litE ($(varE 'integerL) 3)) |]

