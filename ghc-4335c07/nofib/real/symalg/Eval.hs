module Eval (eval,getPrec) where

import BasicNumber
import BasicNumberApprox
import Ast
import Env

-- eval takes an expression and environment, tries to reduce the expression,
-- and returns the reduced expression.
eval :: BasicExp -> Env -> BasicExp
eval (EVar evar) env      = eval (lookupEnv evar env) env
eval (Func name args) env = case args of
				[]          -> Func name []
				[arg]       -> eval_func_1 name arg env
				[arg1,arg2] -> eval_func_2 name arg1 arg2 env
				args	    -> eval_func_n name args env
eval bexp env             = bexp

-- get precision from the environment
getPrec :: Env -> Integer
getPrec env = prec
	where
		prec = read (show bprec)
                bprec = case pexpr of
                            (Numb n) -> -n
                            _        -> -10
                pexpr1 = lookupEnv "$prec" env
                pexpr = eval pexpr1 env

-- evaluate functions with 1 argument.
eval_func_1 :: String -> BasicExp -> Env -> BasicExp
eval_func_1 name arg env =
		if isBuiltin1 name then
			(getBuiltin1 name) narg (getPrec env)
		else	Func name [narg]
		where 
		    narg = eval arg env

-- evaluate functions with 2 arguments.
eval_func_2 :: String -> BasicExp -> BasicExp -> Env -> BasicExp
eval_func_2 name arg1 arg2 env =
		if isBuiltin2 name then
			(getBuiltin2 name narg1 narg2) narg1 narg2 (getPrec env)
		else	Func name [narg1,narg2]
		where
		    narg1 = eval arg1 env
		    narg2 = eval arg2 env

-- evaluate functions with n(n>2) arguments.
eval_func_n :: String -> [BasicExp] -> Env -> BasicExp
eval_func_n name args env = Func name nargs
			where
				nargs = map eval_element args
				eval_element elem = eval elem env

-- test if a function is builtin of arity 1
isBuiltin1 :: String -> Bool
isBuiltin1 "sqrt" = True
isBuiltin1 "real" = True
isBuiltin1 "rat"  = True
isBuiltin1 "neg"  = True
isBuiltin1 _      = False

-- get a builtin function with 1 argument

getBuiltin1 :: String -> (BasicExp -> Integer -> BasicExp)
getBuiltin1 "sqrt" = aBnf2Bef1 "sqrt" sqrt1 where
			sqrt1 :: BasicNumber -> Integer -> BasicNumber
			sqrt1 n _ = sqrt n
getBuiltin1 "real" = aBnf2Bef1 "real" makeReal1 where
			makeReal1 :: BasicNumber -> Integer -> BasicNumber
			makeReal1 n _ = makeReal n
getBuiltin1 "rat"  = aBnf2Bef1 "rat"  rtoRational 
getBuiltin1 "neg"  = aBnf2Bef1 "neg" negation where
			negation :: BasicNumber -> Integer -> BasicNumber
			negation x _ = 0-x

-- convert arithmetic functions on numbers to those on expressions

aBnf2Bef1 :: String -> (BasicNumber -> Integer -> BasicNumber) ->
	    (BasicExp -> Integer -> BasicExp)

aBnf2Bef1 name fun arg prec =
	case arg of
	    (Numb n) -> Numb (fun n prec)
	    _	     -> (Func name [arg])

-- test if a function is builtin of arity 2		
isBuiltin2 :: String -> Bool
isBuiltin2 "add" = True
isBuiltin2 "sub" = True
isBuiltin2 "mul" = True
isBuiltin2 "div" = True
isBuiltin2 "equ" = True
isBuiltin2 "ne"  = True
isBuiltin2 "gte" = True
isBuiltin2 "lte" = True
isBuiltin2 "lt"  = True
isBuiltin2 "gt"  = True
isBuiltin2 _     = False

-- get a builtin function with 2 arguments
getBuiltin2 :: String -> BasicExp -> BasicExp -> 
		(BasicExp -> BasicExp -> Integer -> BasicExp)
getBuiltin2 "add" _	   _	    = aBnf2Bef "add" (+)
getBuiltin2 "sub" _	   _	    = aBnf2Bef "sub" (-)
getBuiltin2 "mul" _	   _	    = aBnf2Bef "mul" (*)
getBuiltin2 "div" _	   _	    = aBnf2Bef "div" (/)
getBuiltin2 "equ" _ _ = bBnf2Bef  "equ" equ
getBuiltin2 "ne"  _ _ = bBnf2Bef  "ne"  ne
getBuiltin2 "lt"  _ _ = bBnf2Bef  "lt"  lt
getBuiltin2 "gt"  _ _ = bBnf2Bef  "gt"  gt
getBuiltin2 "gte" _ _ = bBnf2Bef  "gte" gte
getBuiltin2 "lte" _ _ = bBnf2Bef  "lte" lte

-- convert Haskell boolean to basic expression
bool2bexp :: Bool -> BasicExp
bool2bexp True  = Numb 1
bool2bexp False = Numb 0

-- convert boolean functions on numbers to those on expressions

bBnf2Bef :: String -> (BasicNumber -> BasicNumber -> Integer -> Bool)
	 -> BasicExp -> BasicExp -> Integer -> BasicExp
bBnf2Bef name fun e1 e2 prec = 
	case (e1,e2) of
		((Numb n1),(Numb n2)) -> bool2bexp (fun n1 n2 prec)
		_		      -> (Func name [e1,e2])

-- convert arithmetic functions on numbers to those on expressions

aBnf2Bef :: String -> (BasicNumber -> BasicNumber -> BasicNumber) ->
	    (BasicExp -> BasicExp -> Integer -> BasicExp)
aBnf2Bef name fun arg1 arg2 _ =
	case (arg1,arg2) of
	    ((Numb n1),(Numb n2)) -> Numb (fun n1 n2)
	    _			  -> (Func name [arg1, arg2])
