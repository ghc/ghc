{-# LANGUAGE TemplateHaskell #-}
module T7918A where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

qq = QuasiQuoter {
         quoteExp  = \str -> case str of
                                "e1" -> [| True |]
                                "e2" -> [| id True |]
                                "e3" -> [| True || False |]
                                "e4" -> [| False |]
       , quoteType = \str -> case str of
                                "t1" -> [t| Bool |]
                                "t2" -> [t| Maybe Bool |]
                                "t3" -> [t| Either Bool Int |]
                                "t4" -> [t| Int |]
       , quotePat  = let x = VarP (mkName "x")
                         y = VarP (mkName "y")
                     in \str -> case str of
                                  "p1" -> return $ x
                                  "p2" -> return $ ConP 'Just [x]
                                  "p3" -> return $ TupP [x, y]
                                  "p4" -> return $ y
       , quoteDec  = undefined
       }
