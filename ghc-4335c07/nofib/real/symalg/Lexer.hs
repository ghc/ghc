module Lexer (Lexeme(..), lexer) where

import Data.Char -- 1.3

-- lexeme
data Lexeme = Ide String
	    | Evar String
	    | Op String
	    | Num String
	    | Lparen
	    | Rparen
	    | Comma
  deriving Eq

-- lexical analyzer returns a Bool indicating whether scanning is successful.
lexer :: String -> ([Lexeme], Bool)

lexer ""      = ([], True)
lexer r@(c:s) = 
	if isSpace c      then lexer (dropWhile isSpace s)
	else if isAlpha c then 
                          let (str1,str2) = span isAlphaNum r
                          in current_lexeme (Ide str1) str2
	else if isDigit c then 
                          let (lexeme, rest) = (lexerNum r)
                          in current_lexeme lexeme rest
	else if isOp c    then 
                          let (op, rest) = getOp (c:s)
                          in current_lexeme (Op op) rest
	else if c == '('  then current_lexeme Lparen s
	else if c == ')'  then current_lexeme Rparen s
	else if c == ','  then current_lexeme Comma s
	else if c == '$'  then let (str1,str2) = span isAlphaNum s
                               in current_lexeme (Evar ('$':str1)) str2
	else (consume s, False)
 where
	current_lexeme clex rest = (clex : rlex, succ)
			where (rlex, succ) = lexer rest
	isOp c = c `elem` "+-*/^='><"
	getOp (c:s) = if c `elem` "+-*^" then ([c], s)
		      else case s of
			('=':ss) -> case c of
				        '=' -> ("==",ss)
					'>' -> (">=",ss)
					'<' -> ("<=",ss)
					'/' -> ("/=",ss)
					_   -> ([c],s)
			_	  -> ([c],s)

	lexerNum r = ((Num (ds++f)), t) where (ds,s) = span isDigit r
				              (f,t) = lexFracExp s
	lexFracExp ('.':r) = ('.':ds, s) 
		where (ds, s) = span isDigit r
	lexFracExp s = ("",s)
	consume [] = []
	consume (s:ss) = consume ss
