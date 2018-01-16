-----------------------------------------------------------------------------
The lexer.

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> module Lexer (
>       Token(..),
>       TokenId(..),
>       lexer ) where

> import ParseMonad        

> import Data.Char ( isSpace, isAlphaNum, isDigit, digitToInt )

> data Token 
>       = TokenInfo String TokenId
>       | TokenNum  Int    TokenId
>       | TokenKW          TokenId
>	| TokenEOF

> tokenToId :: Token -> TokenId
> tokenToId (TokenInfo _ i) = i
> tokenToId (TokenNum _ i) = i
> tokenToId (TokenKW i) = i

> instance Eq Token where
>       i == i' = tokenToId i == tokenToId i'

> instance Ord Token where
>       i <= i' = tokenToId i <= tokenToId i'

> data TokenId 
>       = TokId                 -- words and symbols
>       | TokSpecId_TokenType   -- %tokentype
>       | TokSpecId_Token       -- %token
>       | TokSpecId_Name        -- %name
>       | TokSpecId_Partial     -- %partial
>       | TokSpecId_Lexer       -- %lexer
>       | TokSpecId_ImportedIdentity -- %importedidentity
>       | TokSpecId_Monad       -- %monad
>       | TokSpecId_Nonassoc    -- %nonassoc
>       | TokSpecId_Left        -- %left
>       | TokSpecId_Right       -- %right
>       | TokSpecId_Prec        -- %prec
>       | TokSpecId_Expect      -- %expect
>       | TokSpecId_Error       -- %error
>       | TokSpecId_Attributetype -- %attributetype
>       | TokSpecId_Attribute   -- %attribute
>       | TokCodeQuote          -- stuff inside { .. }
>       | TokColon              -- :
>       | TokSemiColon          -- ;
>       | TokDoubleColon        -- ::
>       | TokDoublePercent      -- %%
>       | TokBar                -- |
>       | TokNum                -- Integer
>       deriving (Eq,Ord

#ifdef DEBUG

>       	,Show

#endif

>		)

ToDo: proper text instance here, for use in parser error messages.

> lexer :: (Token -> P a) -> P a
> lexer cont = P lexer'
>   where lexer' "" = returnToken cont TokenEOF ""
>         lexer' ('-':'-':r) = lexer' (dropWhile (/= '\n') r)
>         lexer' ('{':'-':r) = \line -> lexNestedComment line lexer' r line
>         lexer' (c:rest) = nextLex cont c rest

> returnToken cont tok = runP (cont tok)

> nextLex :: (Token -> P a) -> Char -> String -> Int -> ParseResult a
> nextLex cont c = case c of
>  	'\n' 	-> \rest line -> returnToken lexer cont rest (line+1)
>  	'%' 	-> lexPercent cont
>  	':' 	-> lexColon cont
>  	';' 	-> returnToken cont (TokenKW TokSemiColon)

>  	'|' 	-> returnToken cont (TokenKW TokBar)
>  	'\''	-> lexChar cont
>  	'"'{-"-}-> lexString cont
>  	'{' 	-> lexCode cont
>  	c 	
>	  | isSpace c -> runP (lexer cont)
>	  |  c >= 'a' && c <= 'z' 
>	     || c >= 'A' && c <= 'Z' -> lexId cont c
>         | isDigit c -> lexNum cont c
>	c       -> lexError ("lexical error before `" ++ c : "'")

Percents come in two forms, in pairs, or 
followed by a special identifier.

> lexPercent cont s = case s of
> 	'%':rest -> returnToken cont (TokenKW TokDoublePercent) rest
> 	't':'o':'k':'e':'n':'t':'y':'p':'e':rest -> 
>		returnToken cont (TokenKW TokSpecId_TokenType) rest
> 	't':'o':'k':'e':'n':rest ->
> 		returnToken cont (TokenKW TokSpecId_Token) rest
> 	'n':'a':'m':'e':rest ->
> 		returnToken cont (TokenKW TokSpecId_Name) rest
> 	'p':'a':'r':'t':'i':'a':'l':rest ->
> 		returnToken cont (TokenKW TokSpecId_Partial) rest
> 	'i':'m':'p':'o':'r':'t':'e':'d':'i':'d':'e':'n':'t':'i':'t':'y':rest ->
> 		returnToken cont (TokenKW TokSpecId_ImportedIdentity) rest
> 	'm':'o':'n':'a':'d':rest ->
> 		returnToken cont (TokenKW TokSpecId_Monad) rest
> 	'l':'e':'x':'e':'r':rest ->
> 		returnToken cont (TokenKW TokSpecId_Lexer) rest
>       'n':'o':'n':'a':'s':'s':'o':'c':rest ->
>               returnToken cont (TokenKW TokSpecId_Nonassoc) rest
>       'l':'e':'f':'t':rest ->
>               returnToken cont (TokenKW TokSpecId_Left) rest
>       'r':'i':'g':'h':'t':rest ->
>               returnToken cont (TokenKW TokSpecId_Right) rest
>       'p':'r':'e':'c':rest ->
>               returnToken cont (TokenKW TokSpecId_Prec) rest
>       'e':'x':'p':'e':'c':'t':rest ->
>               returnToken cont (TokenKW TokSpecId_Expect) rest
>       'e':'r':'r':'o':'r':rest ->
>               returnToken cont (TokenKW TokSpecId_Error) rest
>       'a':'t':'t':'r':'i':'b':'u':'t':'e':'t':'y':'p':'e':rest ->
>               returnToken cont (TokenKW TokSpecId_Attributetype) rest
>       'a':'t':'t':'r':'i':'b':'u':'t':'e':rest ->
>               returnToken cont (TokenKW TokSpecId_Attribute) rest
>	_ -> lexError ("unrecognised directive: %" ++ 
>				takeWhile (not.isSpace) s) s

> lexColon cont (':':rest) = returnToken cont (TokenKW TokDoubleColon) rest
> lexColon cont rest       = returnToken cont (TokenKW TokColon) rest

> lexId cont c rest = 
>	readId rest (\ id rest' -> returnToken cont (TokenInfo (c:id) TokId) rest')

> lexChar cont rest = lexReadChar rest 
>	(\ id -> returnToken cont (TokenInfo ("'" ++ id ++ "'") TokId))

> lexString cont rest = lexReadString rest 
>	(\ id -> returnToken cont (TokenInfo ("\"" ++ id ++ "\"") TokId))

> lexCode cont rest = lexReadCode rest 0 "" cont

> lexNum cont c rest = 
>        readNum rest (\ num rest' -> 
>                         returnToken cont (TokenNum (stringToInt (c:num)) TokNum) rest')
>  where stringToInt = foldl (\n c -> digitToInt c + 10*n) 0

> cleanupCode s = 
>    dropWhile isSpace (reverse (dropWhile isSpace (reverse s)))

This has to match for @}@ that are {\em not} in strings.  The code
here is a bit tricky, but should work in most cases.

> lexReadCode s n c = case s of
>	'\n':r -> \cont l ->  lexReadCode r n ('\n':c) cont (l+1)
>
> 	'{' :r -> lexReadCode r (n+1) ('{':c)
>
> 	'}' :r
>		| n == 0    -> \cont -> returnToken cont (TokenInfo (
>				cleanupCode (reverse c)) TokCodeQuote) r
>		| otherwise -> lexReadCode r (n-1) ('}':c)
>
> 	'"'{-"-}:r -> lexReadString r (\ str r' -> 
>         	      lexReadCode r' n ('"' : (reverse str) ++ '"' : c))
>
> 	a: '\'':r | isAlphaNum a -> lexReadCode r n ('\'':a:c)
>
> 	'\'' :r	-> lexReadChar r (\ str r' -> 
>         	   lexReadCode r' n ('\'' : (reverse str) ++ '\'' : c))
>
> 	ch:r -> lexReadCode r n (ch:c)
>
> 	[] -> \cont -> lexError "No closing '}' in code segment" []

----------------------------------------------------------------------------
Utilities that read the rest of a token.

> readId :: String -> (String -> String -> a) -> a
> readId (c:r) fn | isIdPart c = readId r (fn . (:) c)
> readId r     fn = fn [] r

> readNum (c:r) fn | isDigit c = readNum r (fn . (:) c)
> readNum r     fn = fn [] r

> isIdPart :: Char -> Bool
> isIdPart c =
>	   c >= 'a' && c <= 'z' 
>	|| c >= 'A' && c <= 'Z' 
>	|| c >= '0' && c <= '9' 
>	|| c == '_'

> lexReadChar :: String -> (String -> String -> a) -> a
> lexReadChar ('\'':r)      fn = fn "" r
> lexReadChar ('\\':'\'':r) fn = lexReadChar r (fn . (:) '\\' . (:) '\'')
> lexReadChar ('\\':c:r)    fn = lexReadChar r (fn . (:) '\\' . (:) c)
> lexReadChar (c:r)         fn = lexReadChar r (fn . (:) c)
> lexReadChar []            fn = fn "" []

> lexReadString :: String -> (String -> String -> a) -> a
> lexReadString ('"'{-"-}:r) fn = fn "" r
> lexReadString ('\\':'"':r) fn = lexReadString r (fn . (:) '\\' . (:) '"')
> lexReadString ('\\':c:r)   fn = lexReadString r (fn . (:) '\\' . (:) c)
> lexReadString (c:r)        fn = lexReadString r (fn . (:) c)
> lexReadString []           fn = fn "" []

> lexError err = runP (lineP >>= \l -> fail (show l ++ ": " ++ err ++ "\n"))

> lexNestedComment l cont r = 
>   case r of
>	'-':'}':r -> cont r
>	'{':'-':r -> \line -> lexNestedComment line 
>			(\r -> lexNestedComment l cont r) r line
>	'\n':r    -> \line -> lexNestedComment l cont r (line+1)
>	c:r       -> lexNestedComment l cont r
>	""	  -> \_ -> lexError "unterminated comment" r l
