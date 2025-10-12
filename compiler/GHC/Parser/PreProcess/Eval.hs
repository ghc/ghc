module GHC.Parser.PreProcess.Eval where

import GHC.Parser.PreProcess.State
import GHC.Prelude

-- ---------------------------------------------------------------------

eval :: Expr -> Int
eval (Parens e) = eval e
eval (Not e) = fromBool $ not (toBool $ eval e)
eval (Var _) = 0 -- Spec says remaining identifiers are replaces with zero
eval (IntVal i) = i
eval (Plus e1 e2) = (eval e1) + (eval e2)
eval (Minus e1 e2) = (eval e1) - (eval e2)
eval (Times e1 e2) = (eval e1) * (eval e2)
eval (Logic op e1 e2) = evalLogicOp op (eval e1) (eval e2)
eval (Comp op e1 e2) = evalCompOp op (eval e1) (eval e2)

evalLogicOp :: LogicOp -> Int -> Int -> Int
evalLogicOp LogicalOr e1 e2 = fromBool $ (toBool e1) || (toBool e2)
evalLogicOp LogicalAnd e1 e2 = fromBool $ (toBool e1) && (toBool e2)

evalCompOp :: CompOp -> Int -> Int -> Int
evalCompOp CmpEqual e1 e2 = fromBool $ e1 == e2
evalCompOp CmpNotEqual e1 e2 = fromBool $ e1 /= e2
evalCompOp CmpGt e1 e2 = fromBool $ e1 > e2
evalCompOp CmpGtE e1 e2 = fromBool $ e1 >= e2
evalCompOp CmpLt e1 e2 = fromBool $ e1 < e2
evalCompOp CmpLtE e1 e2 = fromBool $ e1 <= e2

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
v1 = eval (Comp CmpGt (IntVal 4) (IntVal 3))
