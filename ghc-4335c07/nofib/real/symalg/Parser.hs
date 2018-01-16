module Parser (parse) where

import Ast
import BasicNumber
import Lexer
import Op

-- parse string to ast
parse :: String -> Ast

parse str = if succ then parser lexeme
		    else SyntaxError
	where
		(lexeme, succ) = lexer str

-- parse lexeme list to ast
parser :: [Lexeme] -> Ast

parser lexeme = if rest == [] then ast else SyntaxError
		where (ast,rest) = parse_command lexeme

-- parse a lexeme list, return an ast and the rest of the lexeme list
parse_command :: [Lexeme] -> (Ast, [Lexeme])
parse_command [] = (NullCmd,[])
parse_command ((Evar evar):(Op "="):bexpr) = 
		case bexpr of
		    []              -> (NullCmd,[])
		    (Op "'"):bexpr1 -> ((Set evar ast), rest)
				where (ast,rest) = parse_bexpr bexpr1
		    _          	    -> ((EvalSet evar ast), rest)
				where (ast,rest) = parse_bexpr bexpr
parse_command bexpr = ((Eval ast), rest)
			where
				(ast,rest) = parse_bexpr bexpr

-- parse an expression
parse_bexpr :: [Lexeme] -> (BasicExp, [Lexeme])
parse_bexpr [] = (BSError, [])
parse_bexpr expr = parse_prec 7 expr

parse_prec :: Int -> [Lexeme] -> (BasicExp, [Lexeme])
-- we are now in front of an expression
parse_prec prec rest =
    if prec == 0 then parse_bexpr3 rest
    else 
	case rest of
	    ((Op op):rs) -> if opname == "" then (BSError,rest)
			    else parse_op_acum prec sofar r
			    where 
				(t,r) = parse_prec ((opPrec1 op)-1) rs
				sofar = Func opname [t]
				opname = opName1 op
	    _		 -> parse_op_acum prec t r
			    where
				(t,r) = parse_prec (prec-1) rest
	where
	    parse_op_acum prec sofar r =
                case r of
                    ((Op op):rs) -> if prec >= opPrec op then
                                	let
                                          (s1,r1) = parse_op op sofar rs
                                        in parse_op_acum prec s1 r1
                        	    else (sofar,r)
                    _            -> (sofar,r)

-- in front of an operator
parse_op :: String -> BasicExp -> [Lexeme] -> (BasicExp, [Lexeme])
parse_op op sofar rest =
    if opname == "" then (BSError, rest)
    else
	if opAssoc op == "right" then
		let (t2,r2) = parse_prec (opPrec op) rest
                in ((Func opname [sofar,t2]), r2) 
	else if opAssoc op == "left" then
		parse_left op sofar rest
	     else
	    	parse_non op sofar rest
    where opname = opName op

-- parse operators with no fixity
parse_non :: String -> BasicExp -> [Lexeme] -> (BasicExp, [Lexeme])
parse_non op sofar rest =
	((Func (opName op) [sofar,t2]), r2) 
	where
		(t2,r2) = parse_prec ((opPrec op)-1) rest

-- parsing left-associative operators
parse_left :: String -> BasicExp -> [Lexeme] -> (BasicExp, [Lexeme])
parse_left op sofar rest =
	case r1 of
	    ((Op nop):rs) ->
		if (opPrec op) == (opPrec nop) then
		    parse_left nop nsofar rs
		else
		    (nsofar,r1)
--		    parse_op nop (Func (opName op) [sofar,t1]) rs
	    _ 		  -> (nsofar,r1)
	where
	    (t1,r1) = parse_prec ((opPrec op)-1) rest
	    nsofar = Func (opName op) [sofar,t1]

-- atomic expression
parse_bexpr3 :: [Lexeme] -> (BasicExp, [Lexeme])
parse_bexpr3 ((Evar evar):rest)      = ((EVar evar), rest)
parse_bexpr3 ((Ide var):Lparen:rest) = 
			if succ then ((Func var args), r)
				else (BSError,r)
			where
				(args,r,succ) = parse_arglist [] rest
parse_bexpr3 ((Ide var):rest)       = ((Var var), rest)
parse_bexpr3 ((Num num):rest)       = ((Numb (read num)), rest)
parse_bexpr3 (Lparen:rest)          = case r1 of
				        (Rparen:r2) -> (exp,r2)
				        _	    -> (BSError,r1)
				      where
					(exp,r1) = parse_bexpr rest
parse_bexpr3 x 			    = (BSError,x)

-- parse argument list
parse_arglist :: [BasicExp] -> [Lexeme] -> ([BasicExp], [Lexeme], Bool)
parse_arglist acum (Rparen:x) = (acum, x, True)
parse_arglist acum x = case r1 of
			(Comma:rs)  -> parse_arglist (acum++[arg]) rs
			(Rparen:rs) -> (acum++[arg],rs,True)
			_	    -> ([],[],False) 
		where
			(arg,r1) = parse_bexpr x
