{-# LANGUAGE TemplateHaskell #-}

-- Used by QuasiQuote. Example taken from the GHC documentation.
module QuasiExpr where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Typeable
import Data.Generics

data Expr  =  IntExpr Integer
           |  AntiIntExpr String
           |  BinopExpr BinOp Expr Expr
           |  AntiExpr String
    deriving(Show, Typeable, Data)

data BinOp  =  AddOp
            |  SubOp
            |  MulOp
            |  DivOp
    deriving(Show, Typeable, Data)

eval :: Expr -> Integer
eval (IntExpr n)        = n
eval (BinopExpr op x y) = (opToFun op) (eval x) (eval y)
  where
    opToFun AddOp = (+)
    opToFun SubOp = (-)
    opToFun MulOp = (*)
    opToFun DivOp = div

expr = QuasiQuoter parseExprExp undefined

-- cheating...
parseExprExp :: String -> Q Exp
parseExprExp _ = [| BinopExpr AddOp (IntExpr 1) (IntExpr 2) |]
