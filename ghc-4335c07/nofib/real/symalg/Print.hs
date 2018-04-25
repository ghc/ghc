module Print (printAst, printBasicExp, printEnv) where

import BasicNumber
import BasicNumberApprox
import Ast
import Op
import Parser
import Env
import Eval
import Data.Ratio

-- print an abstract syntax tree
printAst :: Ast -> Env -> String
printAst (Set name bexp) env = "Set "++name++" to "++(printBasicExp bexp env)
printAst (EvalSet name bexp) env = 
		"Eval\t"++(printBasicExp bexp env)++"\n"++
		"Set\t"++name++" to "
printAst (Eval bexp) env = "Eval "++(printBasicExp bexp env)
printAst SyntaxError _ = "Syntax error"
printAst NullCmd _ = ""

-- print a basic expression
printBasicExp :: BasicExp -> Env -> String
printBasicExp = printExp 10

-- print an expression with a precedence argument
printExp :: Int -> BasicExp -> Env -> String
printExp p (Func name args) env =
		case opname of
		    ""	-> name++"("++(printArgs args env)++")"
		    (x:xs) -> printOp p opname args env
		where
		    opname = case args of
				[arg1,arg2] -> toOp name
				[arg] 	    -> toOp1 name
				_	    -> ""
printExp _ (Numb n) env = mybasicNumber2str n (getPrec env)
printExp _ (Var s) _ = s
printExp _ (EVar s) _ = s
printExp _ BSError _ = "ERROR"

-- print the arguments
printArgs :: [BasicExp] -> Env -> String
printArgs [] _ = ""
printArgs [x] env = printBasicExp x env
printArgs (x:xs) env = (printBasicExp x env)++(printArglist xs env)

-- auxilury funtion for printing the arguments
printArglist :: [BasicExp] -> Env -> String
printArglist [] _ = ""
printArglist (x:xs) env = ","++(printBasicExp x env)++(printArglist xs env)

-- print expression with operators
printOp :: Int -> String -> [BasicExp] -> Env -> String

printOp p opname [arg] env =
        if mp>p then "("++res++")" else res
        where
                res = opname ++ (printExp mp arg env)
                mp = opPrec1 opname

printOp p opname [arg1,arg2] env =
        if mp>=p then res2
                else res1
        where
                lmp = if massoc == "left" then mp+1 else mp
                rmp = if massoc == "right" then mp+1 else mp
                res1 = str1++opname++str2
                res2 = "("++res1++")"
                mp   = opPrec opname
                massoc = opAssoc opname
                str1 = printExp lmp arg1 env
                str2 = printExp rmp arg2 env

printOp _ _ _ _ = ""

-- print an environment
printEnv :: Env -> Env -> String
printEnv [] _ = ""
printEnv (e:es) env = pEnv e env ++ (printEnv es env)
	where
		pEnv e@(str, bexp) env = 
			str++"\t"++(printBasicExp bexp env)++"\n"

mybasicNumber2str :: BasicNumber -> Integer -> String
mybasicNumber2str (BasRationalC x) p =
	if numerator sx == 0 then "0"
	else if denominator sx == 1 then show (numerator sx) 
	     else "("++(basicNumber2str (BasRationalC sx) p)++")"
		where sx = x/1
mybasicNumber2str x p = basicNumber2str x p 
