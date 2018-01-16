module Ast where

import BasicNumber

type VarName = String

type FunName = String

-- abstract syntax tree
data Ast = Set VarName BasicExp		-- set variable literally
	 | EvalSet VarName BasicExp	-- eval and set
	 | Eval BasicExp		-- eval
	 | NullCmd			-- null command
	 | SyntaxError			-- syntax error

-- basic expression
data BasicExp = Func FunName [BasicExp]
	      | Numb BasicNumber
	      | Var String
	      | EVar String
	      | BSError
