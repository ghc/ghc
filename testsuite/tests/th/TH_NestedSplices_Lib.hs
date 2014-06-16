{-# LANGUAGE TemplateHaskell #-}
module TH_NestedSplices_Lib where

import Language.Haskell.TH

spliceExpr :: String -> Q Exp -> Q Exp
spliceExpr s e = [| (s, $e) |]

declareFun :: String -> Q [Dec]
declareFun s
  = do { n <- newName s
       ; d <- funD n [clause [] (normalB [| 22 |]) []]
       ; return [d] }

