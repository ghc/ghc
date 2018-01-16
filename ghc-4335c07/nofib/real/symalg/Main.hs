module Main where

import Ast
import Parser
import Env
import Print
import Eval
import BasicNumber -- partain
import System.IO

----------------------------------------------------------------------------

-- command line prompt
prompt :: String
prompt = "-> "
----------------------------------------------------------------------------

main :: IO ()
main = cmdloop (initEnv [])
  where
    cmdloop env = do
        putStr prompt
        hFlush stdout
        l <- getLine
        if l == "exit" then 
	   return ()
	 else do
           let (res, nenv) = cmd_process (l, env)
	   putStrLn res
	   cmdloop nenv

----------------------------------------------------------------------------

-- process the command line
cmd_process :: (String, Env) -> (String, Env)
cmd_process (c,e) = 
	case c of
	"env"   -> ((printEnv e e),e)
	"clear" -> ("",initEnv [])
	"?"	-> (printHelp, e)
	_	-> 
		case ast of
		(Set evar bexpr)     -> (printAst ast e, enterEnv evar bexpr e)
		(EvalSet evar bexpr) -> (res, enterEnv evar rexpr e)
				where 	rexpr = eval bexpr e
					res = printAst ast e ++
					      (printBasicExp rexpr e)
		(Eval bexpr) 	     -> (printBasicExp (eval bexpr e) e, e)
		_	     	     -> (printAst ast e, e)
	    where ast = parse c

----------------------------------------------------------------------------

-- print the help menu
printHelp :: String
printHelp = "Commands:\n"++
	    "clear        clear the environment\n"++
	    "env          browse through the environment\n"++
	    "exit         quit\n"++
	    "?            display this help info\n"++
	    "$var = 'expr augment the environment\n"++
	    "$var = expr  eval and assign\n"++
	    "expr         evaluate the expression\n\n"++ 
  "where expr could be any of the following:\n\n"++
  "EXP -> BINARY-EXP (EXP1,EXP2)\n"++
  "     | UNARY-EXP (EXP)\n"++
  "     | EXP1 OP EXP2\n"++
  "\n"++
  "BINARY-EXP -> add | sub | mul | div | equ\n"++
  "            | ne  | lt  | gt | lte | gte\n"++
  "\n"++
  "UNARY-EXP -> sqrt | real | rat \n"++
  "\n"++
  "VAR -> a variable name\n"
----------------------------------------------------------------------------
