module Eval where

import Parse

-- ---------------------------------------------------------------------

eval :: Expr -> Int
eval (Parens e) = eval e
eval (Var v) = error $ "need to look up :" ++ v
eval (IntVal i) = i
eval (Plus e1 e2) = (eval e1) + (eval e2)
eval (Times e1 e2) = (eval e1) * (eval e2)
eval (BinOp op e1 e2) = evalOp op (eval e1) (eval e2)

evalOp :: Op -> Int -> Int -> Int
evalOp LogicalOr e1 e2 = fromBool $ (toBool e1) || (toBool e2)
evalOp LogicalAnd e1 e2 = fromBool $ (toBool e1) || (toBool e2)
evalOp CmpEqual e1 e2 = fromBool $ e1 == e2
evalOp CmpGt e1 e2 = fromBool $ e1 > e2
evalOp CmpGtE e1 e2 = fromBool $ e1 >= e2
evalOp CmpLt e1 e2 = fromBool $ e1 < e2
evalOp CmpLtE e1 e2 = fromBool $ e1 <= e2

toBool :: Int -> Bool
toBool 0 = False
toBool _ = True

fromBool :: Bool -> Int
fromBool False = 0
fromBool True = 1

-- ---------------------------------------------------------------------

v0 :: Int
v0 = eval (Plus (IntVal 1) (IntVal 3))

v1 :: Int
v1 = eval (BinOp CmpGt (IntVal 4) (IntVal 3))
