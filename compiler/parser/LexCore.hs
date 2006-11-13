module LexCore where

import ParserCoreUtils
import Ratio
import Char
import Numeric

isNameChar c = isAlpha c || isDigit c || (c == '_') || (c == '\'')
	       || (c == '$') || (c == '-') || (c == '.')
isKeywordChar c = isAlpha c || (c == '_') 

lexer :: (Token -> P a) -> P a 
lexer cont [] 		= cont TKEOF []
lexer cont ('\n':cs) 	= \line -> lexer cont cs (line+1)
lexer cont ('-':'>':cs) = cont TKrarrow cs

lexer cont (c:cs) 
      | isSpace c		= lexer cont cs
      | isLower c || (c == '_') = lexName cont TKname (c:cs)
      | isUpper c 		= lexName cont TKcname (c:cs)
      | isDigit c || (c == '-') = lexNum cont (c:cs)

lexer cont ('%':cs)  	= lexKeyword cont cs
lexer cont ('\'':cs) 	= lexChar cont cs
lexer cont ('\"':cs) 	= lexString [] cont cs 
lexer cont ('#':cs) 	= cont TKhash cs
lexer cont ('(':cs) 	= cont TKoparen cs
lexer cont (')':cs) 	= cont TKcparen cs
lexer cont ('{':cs) 	= cont TKobrace cs
lexer cont ('}':cs) 	= cont TKcbrace cs
lexer cont ('=':cs)     = cont TKeq cs
lexer cont (':':'=':':':cs) = cont TKcoloneqcolon cs
lexer cont (':':':':cs) = cont TKcoloncolon cs
lexer cont ('*':cs) 	= cont TKstar cs
lexer cont ('.':cs) 	= cont TKdot cs
lexer cont ('\\':cs)    = cont TKlambda cs
lexer cont ('@':cs) 	= cont TKat cs
lexer cont ('?':cs) 	= cont TKquestion cs
lexer cont (';':cs) 	= cont TKsemicolon cs
-- 20060420 GHC spits out constructors with colon in them nowadays. jds
-- 20061103 but it's easier to parse if we split on the colon, and treat them
-- as several tokens
lexer cont (':':cs)     = cont TKcolon cs
-- 20060420 Likewise does it create identifiers starting with dollar. jds
lexer cont ('$':cs)     = lexName cont TKname ('$':cs)
lexer cont (c:cs) 	= failP "invalid character" [c]



lexChar cont ('\\':'x':h1:h0:'\'':cs)
	| isHexEscape [h1,h0] =  cont (TKchar (hexToChar h1 h0)) cs
lexChar cont ('\\':cs) 	 = failP "invalid char character" ('\\':(take 10 cs))
lexChar cont ('\'':cs) 	 = failP "invalid char character" ['\'']
lexChar cont ('\"':cs) 	 = failP "invalid char character" ['\"']
lexChar cont (c:'\'':cs) = cont (TKchar c) cs


lexString s cont ('\\':'x':h1:h0:cs) 
	| isHexEscape [h1,h0] = lexString (s++[hexToChar h1 h0]) cont cs
lexString s cont ('\\':cs) = failP "invalid string character" ['\\']
lexString s cont ('\'':cs) = failP "invalid string character" ['\'']
lexString s cont ('\"':cs) = cont (TKstring s) cs
lexString s cont (c:cs) = lexString (s++[c]) cont cs

isHexEscape = all (\c -> isHexDigit c && (isDigit c || isLower c))

hexToChar h1 h0 = chr (digitToInt h1 * 16 + digitToInt h0)


lexNum cont cs =
  case cs of
     ('-':cs) -> f (-1) cs
     _ 	      -> f 1 cs
 where f sgn cs = 
         case span isDigit cs of
          (digits,'.':c:rest) 
		| isDigit c -> cont (TKrational (fromInteger sgn * r)) rest'
	        where ((r,rest'):_) = readFloat (digits ++ ('.':c:rest))
		-- When reading a floating-point number, which is
		-- a bit complicated, use the Haskell 98 library function
          (digits,rest) -> cont (TKinteger (sgn * (read digits))) rest

lexName cont cstr cs = cont (cstr name) rest
   where (name,rest) = span isNameChar cs

lexKeyword cont cs = 
   case span isKeywordChar cs of
      ("module",rest) -> cont TKmodule rest
      ("data",rest)  -> cont TKdata rest
      ("newtype",rest) -> cont TKnewtype rest
      ("forall",rest) -> cont TKforall rest	
      ("rec",rest) -> cont TKrec rest	
      ("let",rest) -> cont TKlet rest	
      ("in",rest) -> cont TKin rest	
      ("case",rest) -> cont TKcase rest	
      ("of",rest) -> cont TKof rest	
      ("cast",rest) -> cont TKcast rest	
      ("note",rest) -> cont TKnote rest	
      ("external",rest) -> cont TKexternal rest
      ("local",rest) -> cont TKlocal rest
      ("_",rest) -> cont TKwild rest
      _ -> failP "invalid keyword" ('%':cs) 

