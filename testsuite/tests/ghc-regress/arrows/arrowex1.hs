{-# OPTIONS -farrows #-}

-- Toy lambda-calculus interpreter from John Hughes's arrows paper (s5)

module Main(main) where

import Data.Maybe(fromJust)
import Control.Arrow

type Id = String
data Val a = Num Int | Bl Bool | Fun (a (Val a) (Val a))
data Exp = Var Id | Add Exp Exp | If Exp Exp Exp | Lam Id Exp | App Exp Exp

eval :: (ArrowChoice a, ArrowApply a) => Exp -> a [(Id, Val a)] (Val a)
eval (Var s) = proc env ->
		returnA -< fromJust (lookup s env)
eval (Add e1 e2) = proc env -> do
		~(Num u) <- eval e1 -< env
		~(Num v) <- eval e2 -< env
		returnA -< Num (u + v)
eval (If e1 e2 e3) = proc env -> do
		~(Bl b) <- eval e1 -< env
		if b	then eval e2 -< env
			else eval e3 -< env
eval (Lam x e) = proc env ->
		returnA -< Fun (proc v -> eval e -< (x,v):env)
eval (App e1 e2) = proc env -> do
		~(Fun f) <- eval e1 -< env
		v <- eval e2 -< env
		f -<< v

-- some tests

i = Lam "x" (Var "x")
k = Lam "x" (Lam "y" (Var "x"))
double = Lam "x" (Add (Var "x") (Var "x"))

-- if b then k (double x) x else x + x + x

text_exp = If (Var "b")
		(App (App k (App double (Var "x"))) (Var "x"))
		(Add (Var "x") (Add (Var "x") (Var "x")))

unNum (Num n) = n

main = do
	print (unNum (eval text_exp [("b", Bl True), ("x", Num 5)]))
	print (unNum (eval text_exp [("b", Bl False), ("x", Num 5)]))
