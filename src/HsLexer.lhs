-----------------------------------------------------------------------------
-- $Id: HsLexer.lhs,v 1.1 2002/04/04 16:23:43 simonmar Exp $
--
-- (c) The GHC Team, 1997-2000
--
-- Lexer for Haskell.
--
-----------------------------------------------------------------------------

ToDo: Parsing floats is a *real* hack...
ToDo: Introduce different tokens for decimal, octal and hexadecimal (?)
ToDo: FloatTok should have three parts (integer part, fraction, exponent)
ToDo: Use a lexical analyser generator (lx?)

\begin{code}
module HsLexer (Token(..), lexer, parseError,isSymbol) where

import HsParseMonad
import HsParseUtils
import HsSyn(SrcLoc(..))

import Numeric	( readHex, readOct )
import Char
\end{code}

\begin{code}
data Token 
        = VarId String
        | QVarId (String,String)
	| ConId String
        | QConId (String,String)
        | VarSym String
        | ConSym String
        | QVarSym (String,String)
        | QConSym (String,String)

-- Literals

	| IntTok     Integer
        | FloatTok   String
	| Character  Char
        | StringTok  String
	| PrimChar   Char		-- GHC extension
	| PrimInt    Integer		-- GHC extension
        | PrimString String		-- GHC extension
	| PrimFloat  String		-- GHC extension
	| PrimDouble String		-- GHC extension

-- Symbols

	| LeftParen
	| RightParen
	| SemiColon
        | LeftCurly
        | RightCurly
        | VRightCurly			-- a virtual close brace
        | LeftSquare
        | RightSquare
	| Comma
        | Underscore
        | BackQuote
	| LeftUT			-- GHC Extension: (#
	| RightUT			-- GHC Extension: #)
	
-- Documentation annotations

	| DocCommentNext String		-- something beginning '-- |'
	| DocCommentPrev String		-- something beginning '-- ^'
	| DocCommentNamed String	-- something beginning '-- @'
	| DocSection Int String		-- a section heading

-- Reserved operators

	| Dot				-- GHC extension
	| DotDot
	| DoubleColon
	| Equals
	| Backslash
	| Bar
	| LeftArrow
	| RightArrow
	| At
	| Tilde
	| DoubleArrow
	| Minus
	| Exclamation

-- Reserved Ids

	| KW_As
	| KW_Case
	| KW_CCall
	| KW_Class
	| KW_Data
	| KW_Default
	| KW_Deriving
	| KW_Do
	| KW_DotNet
	| KW_Else
	| KW_Export
	| KW_Forall
	| KW_Foreign
        | KW_Hiding
	| KW_If
	| KW_Import
	| KW_In
	| KW_Infix
	| KW_InfixL
	| KW_InfixR
	| KW_Instance
	| KW_Let
	| KW_Module
	| KW_NewType
	| KW_Of
	| KW_Safe
	| KW_StdCall
	| KW_Then
	| KW_ThreadSafe
	| KW_Type
	| KW_Unsafe
	| KW_Where    
	| KW_Qualified

        | EOF
        deriving (Eq,Show)

reserved_ops :: [(String,Token)]
reserved_ops = [
 ( ".",  Dot ),				-- GHC extension
 ( "..", DotDot ),    
 ( "::", DoubleColon ),
 ( "=",  Equals ),    
 ( "\\", Backslash ), 
 ( "|",  Bar ),       
 ( "<-", LeftArrow ), 
 ( "->", RightArrow ),
 ( "@",  At ),        
 ( "~",  Tilde ),     
 ( "=>", DoubleArrow ),
 ( "-",  Minus ),			--ToDo: shouldn't be here
 ( "!",  Exclamation )		--ditto
 ]

reserved_ids :: [(String,Token)]
reserved_ids = [
 ( "_",         Underscore ),
 ( "case",      KW_Case ),     
 ( "ccall",     KW_CCall ),
 ( "class",     KW_Class ),    
 ( "data",      KW_Data ),     
 ( "default",   KW_Default ),  
 ( "deriving",  KW_Deriving ), 
 ( "do",        KW_Do ),       
 ( "dotnet",    KW_DotNet ),       
 ( "else",      KW_Else ),     
 ( "export",    KW_Export ),     
 ( "forall",    KW_Forall ),     
 ( "foreign",   KW_Foreign ),     
 ( "if",    	KW_If ),       
 ( "import",    KW_Import ),   
 ( "in", 	KW_In ),       
 ( "infix", 	KW_Infix ),    
 ( "infixl", 	KW_InfixL ),   
 ( "infixr", 	KW_InfixR ),   
 ( "instance",  KW_Instance ), 
 ( "let", 	KW_Let ),      
 ( "module", 	KW_Module ),   
 ( "newtype",   KW_NewType ),  
 ( "of", 	KW_Of ),       
 ( "safe", 	KW_Safe ),     
 ( "then", 	KW_Then ),     
 ( "threadsafe",KW_ThreadSafe ),     
 ( "type", 	KW_Type ),     
 ( "unsafe", 	KW_Unsafe ),
 ( "where", 	KW_Where ),    
 ( "as", 	KW_As ),       
 ( "qualified", KW_Qualified ),
 ( "hiding", 	KW_Hiding )
 ]

isIdent  c = isAlpha c || isDigit c || c == '\'' || c == '_'
isSymbol c = elem c ":!#$%&*+./<=>?@\\^|-~"
isWhite  c = elem c " \n\r\t\v\f"

tAB_LENGTH = 8 :: Int

-- The source location, (y,x), is the coordinates of the previous token.
-- col is the current column in the source file.  If col is 0, we are
-- somewhere at the beginning of the line before the first token.

-- Setting col to 0 is used in two places: just after emitting a virtual
-- close brace due to layout, so that next time through we check whether
-- we also need to emit a semi-colon, and at the beginning of the file,
-- to kick off the lexer.


lexer :: (Token -> P a) -> P a
lexer cont input (SrcLoc _ x) y col =
        if col == 0
           then tab y x True  input
           else tab y col False input -- throw away old x
  where
   	-- move past whitespace and comments
        tab y x bol [] = 
        	cont EOF [] (SrcLoc y x) col y
        tab y x bol ('\t':s) =
        	tab y (nextTab x) bol s
        tab y x bol ('\n':s) =
                newLine cont s y
        tab y x bol ('-':'-':s) | not (doc s) =
        	newLine cont (drop 1 (dropWhile (/= '\n') s)) y
        tab y x bol ('{':'-':s) = nestedComment tab y x bol s
        tab y x bol (c:s)
        	| isWhite c = tab y (x+1) bol s
        	| otherwise = 
        		if bol 	then lexBOL   cont (c:s) (SrcLoc y x) y x
        			else lexToken cont (c:s) (SrcLoc y x) y x

	newLine cont s y =  tab (y+1) 1 True s

	doc (' ':'|':_) = True
	doc (' ':'^':_) = True
	doc (' ':'*':_) = True
	doc _ = False

nextTab x = x + (tAB_LENGTH - (x-1) `mod` tAB_LENGTH)

-- When we are lexing the first token of a line, check whether we need to
-- insert virtual semicolons or close braces due to layout.

lexBOL :: (Token -> P a) -> P a
lexBOL cont s loc y x context =
        if need_close_curly then 
                -- trace "layout: inserting '}'\n" $
        	-- Set col to 0, indicating that we're still at the
        	-- beginning of the line, in case we need a semi-colon too.
        	-- Also pop the context here, so that we don't insert
        	-- another close brace before the parser can pop it.
        	cont VRightCurly s loc y 0 (tail context)
        else if need_semi_colon then
                --trace "layout: inserting ';'\n" $
        	cont SemiColon s loc y x context
        else
        	lexToken cont s loc y x context
 where
        need_close_curly =
        	case context of
        		[] -> False
        		(i:_) -> case i of
        			    NoLayout -> False
        			    Layout n -> x < n
        need_semi_colon =
        	case context of
        		[] -> False
        		(i:_) -> case i of
        			    NoLayout -> False
        			    Layout n -> x == n

lexToken :: (Token -> P a) -> P a
lexToken cont s loc y x =
   -- trace ("lexer: y="++show y++" x="++show x++"\n") $ 
   case s of
        -- First the special symbols
        '(':'#':s -> forward 2 LeftUT s
	'(':s 	  -> forward 1 LeftParen s
	'#':')':s -> forward 2 RightUT s
        ')':s     -> forward 1 RightParen s
        ',':s     -> forward 1 Comma s
        ';':s     -> forward 1 SemiColon s
        '[':s     -> forward 1 LeftSquare s
        ']':s     -> forward 1 RightSquare s
        '`':s     -> forward 1 BackQuote s
        '{':s     -> \ctxt -> forward 1 LeftCurly s (NoLayout : ctxt)
        '}':s     -> \ctxt -> case ctxt of
                              (_:ctxt) -> forward 1 RightCurly s ctxt
						-- pop context on '}'
                              []       -> error "Internal error: empty context in lexToken"

	'-':'-':' ':'|':s -> docComment DocCommentNext cont s loc y x
	'-':'-':' ':'^':s -> docComment DocCommentPrev cont s loc y x
	'-':'-':' ':'*':s -> docSection cont ('*':s) loc y x

        '\'':s -> lexChar cont s loc y (x+1)
        '\"':s{-"-} -> lexString cont s loc y (x+1)

        '0':'x':c:s | isHexDigit c -> 
	   let (num, rest) = span isHexDigit (c:s)
	       [(i,_)] = readHex num
	   in
	   afterNum cont i rest loc y (x+length num)
        '0':'o':c:s | isOctDigit c -> 
	   let (num, rest) = span isOctDigit (c:s)
	       [(i,_)] = readOct num
	   in
	   afterNum cont i rest loc y (x+length num)

        c:s | isLower c || c == '_' ->
        	let 
        	    (idtail, rest) = slurpIdent s
        	    id = c:idtail
        	    l_id = 1 + length idtail
        	in
        	case lookup id reserved_ids of
        		Just keyword -> forward l_id keyword rest
        		Nothing -> forward l_id (VarId id) rest

          | isUpper c -> lexCon "" cont (c:s) loc y x
          | isSymbol c ->
        	let
        	    (symtail, rest) = span isSymbol s
        	    sym = c : symtail
        	    l_sym = 1 + length symtail
        	in
        	case lookup sym reserved_ops of
        	    Just t  -> forward l_sym t rest
        	    Nothing -> case c of
        			':' -> forward l_sym (ConSym sym) rest
        			_   -> forward l_sym (VarSym sym) rest

          | isDigit c -> lexNum cont c s loc y x

          | otherwise ->
        	parseError ("illegal character \'" ++ show c ++ "\'\n") 
        		  s loc y x

 where forward n t s = cont t s loc y (x+n)

lexToken _ _ _ _ _ = error "Internal error: empty input in lexToken"

afterNum cont i ('#':s) loc y x = cont (PrimInt i) s loc y (x+1)
afterNum cont i s loc y x = cont (IntTok i) s loc y x

lexNum cont c s loc y x = 
  let (num, after_num) = span isDigit (c:s)
  in
  case after_num of
    '.':c:s | isDigit c ->
	let (frac,after_frac) = span isDigit s
	in
	let float = num ++ '.':frac
	    (f, after_exp)
		 = case after_frac of
		    'E':s -> do_exponent s
		    'e':s -> do_exponent s
		    _     -> (float, after_frac)

	    do_exponent s =
		 case s of
		  '-':c:s | isDigit c -> 
			let (exp,rest) = span isDigit (c:s) in
			(float ++ 'e':'-':exp, rest)
		  '+':c:s | isDigit c -> 
			let (exp,rest) = span isDigit (c:s) in
			(float ++ 'e':'+':exp, rest)
		  c:s | isDigit c -> 
			let (exp,rest) = span isDigit (c:s) in
			(float ++ 'e':exp, rest)
		  _ -> (float, after_frac)

	    x' = x + length f

	in case after_exp of -- glasgow exts only
		'#':'#':s -> cont (PrimDouble f) s loc y x'
		'#':s     -> cont (PrimFloat f)  s loc y x'
		s         -> cont (FloatTok f)   s loc y x'

    _ -> afterNum cont (parseInteger 10 num) after_num loc y (x + length num)

		
-- GHC extension: allow trailing '#'s in an identifier. 
slurpIdent s = slurp' s []
 where
  slurp' [] i = (reverse i, [])
  slurp' (c:cs) i 
    | isIdent c = slurp' cs (c:i)
    | c == '#'  = slurphashes cs (c:i)
  slurp' cs i = (reverse i, cs)

slurphashes [] i = (reverse i, [])
slurphashes ('#':cs) i = slurphashes cs ('#':i)
slurphashes s i = (reverse i, s)


lexCon qual cont s loc y x =
  let
    forward n t s = cont t s loc y (x+n)

    (con, rest) = slurpIdent s
    l_con = length con

    just_a_conid 
	| null qual = forward l_con (ConId con) rest
	| otherwise = forward l_con (QConId (qual,con)) rest
  in
  case rest of
    '.':c1:s1 
     | isLower c1 ->	-- qualified varid?
	let
	    (idtail, rest1) = slurpIdent s1
	    id = c1:idtail
	    l_id = 1 + length idtail
	in
	case lookup id reserved_ids of
	   -- cannot qualify a reserved word
	   Just keyword -> just_a_conid
	   Nothing -> forward (l_con+1+l_id) (QVarId (con, id)) rest1

     | isUpper c1 ->	-- qualified conid?
        let qual' | null qual = con
		  | otherwise = qual ++ '.':con
	in
	lexCon qual' cont (c1:s1) loc y (x+l_con+1)

     | isSymbol c1 ->	-- qualified symbol?
	let
	    (symtail, rest1) = span isSymbol s1
	    sym = c1 : symtail
	    l_sym = 1 + length symtail
	in
	case lookup sym reserved_ops of
	    -- cannot qualify a reserved operator
	    Just _  -> just_a_conid
	    Nothing -> case c1 of
			':' -> forward (l_con+1+l_sym) 
				(QConSym (con, sym)) rest1
			_   -> forward (l_con+1+l_sym)
				(QVarSym (con, sym)) rest1

    _ -> just_a_conid -- not a qualified thing


lexChar :: (Token -> P a) -> P a
lexChar cont s loc y x = case s of
                    '\\':s -> (escapeChar s `thenP` \(e,s,i) _ _ _ _ ->
                               charEnd e s loc y (x+i)) s loc y x
                    c:s    -> charEnd c s loc y (x+1)
                    []     -> error "Internal error: lexChar"

  where charEnd c ('\'':'#':s) = \loc y x -> cont (PrimChar c) s loc y (x+2)
	charEnd c ('\'':s) = \loc y x -> cont (Character c) s loc y (x+1)
        charEnd c s = parseError "Improperly terminated character constant" s

lexString :: (Token -> P a) -> P a
lexString cont s loc y x = loop "" s x y
  where
     loop e s x y = case s of
            '\\':'&':s  -> loop e s (x+2) y
            '\\':c:s | isSpace c -> stringGap e s (x+2) y
        	     | otherwise -> (escapeChar (c:s) `thenP` \(e',s,i) _ _ _ _ ->
        		             loop (e':e) s (x+i) y) s loc y x
            '\"':s{-"-} -> cont (StringTok (reverse e)) s loc y (x+1)
            c:s		-> loop (c:e) s (x+1) y
            []          -> parseError "Improperly terminated string" s loc y x

     stringGap e s x y = case s of
        	'\n':s -> stringGap e s 1 (y+1)
        	'\\':s -> loop e s (x+1) y
        	c:s' | isSpace c -> stringGap e s' (x+1) y
        	     | otherwise -> 
        	       parseError "Illegal character in string gap" s loc y x
                []     -> error "Internal error: stringGap"

-- ToDo: \o, \x, \<octal> things.

escapeChar :: String -> P (Char,String,Int)
escapeChar s = case s of

  'x':c:s | isHexDigit c -> 
	let (num,rest) = span isHexDigit (c:s) in
	returnP (chr (fromIntegral (parseInteger 16 num)), rest, length num)

  'o':c:s | isOctDigit c -> 
	let (num,rest) = span isOctDigit (c:s) in
	returnP (chr (fromIntegral (parseInteger 8 num)), rest, length num)

  c:s | isDigit c -> let (num,rest) = span isDigit (c:s) in
		     returnP (chr (read num), rest, length num)

-- Production charesc from section B.2 (Note: \& is handled by caller)

  'a':s 	  -> returnP ('\a',s,2)
  'b':s 	  -> returnP ('\b',s,2)
  'f':s 	  -> returnP ('\f',s,2)
  'n':s 	  -> returnP ('\n',s,2)
  'r':s 	  -> returnP ('\r',s,2)
  't':s 	  -> returnP ('\t',s,2)
  'v':s 	  -> returnP ('\v',s,2)
  '\\':s        -> returnP ('\\',s,2)
  '"':s         -> returnP ('\"',s,2)
  '\'':s        -> returnP ('\'',s,2)

-- Production ascii from section B.2

  '^':x@(c:s)   -> cntrl x
  'N':'U':'L':s -> returnP ('\NUL',s,4)
  'S':'O':'H':s -> returnP ('\SOH',s,4)
  'S':'T':'X':s -> returnP ('\STX',s,4)
  'E':'T':'X':s -> returnP ('\ETX',s,4)
  'E':'O':'T':s -> returnP ('\EOT',s,4)
  'E':'N':'Q':s -> returnP ('\ENQ',s,4)
  'A':'C':'K':s -> returnP ('\ACK',s,4)
  'B':'E':'L':s -> returnP ('\BEL',s,4)
  'B':'S':s     -> returnP ('\BS', s,3)
  'H':'T':s  	  -> returnP ('\HT', s,3)
  'L':'F':s 	  -> returnP ('\LF', s,3)
  'V':'T':s 	  -> returnP ('\VT', s,3)
  'F':'F':s 	  -> returnP ('\FF', s,3)
  'C':'R':s 	  -> returnP ('\CR', s,3)
  'S':'O':s 	  -> returnP ('\SO', s,3)
  'S':'I':s 	  -> returnP ('\SI', s,3)
  'D':'L':'E':s -> returnP ('\DLE',s,4)
  'D':'C':'1':s -> returnP ('\DC1',s,4)
  'D':'C':'2':s -> returnP ('\DC2',s,4)
  'D':'C':'3':s -> returnP ('\DC3',s,4)
  'D':'C':'4':s -> returnP ('\DC4',s,4)
  'N':'A':'K':s -> returnP ('\NAK',s,4)
  'S':'Y':'N':s -> returnP ('\SYN',s,4)
  'E':'T':'B':s -> returnP ('\ETB',s,4)
  'C':'A':'N':s -> returnP ('\CAN',s,4)
  'E':'M':s     -> returnP ('\EM', s,3)
  'S':'U':'B':s -> returnP ('\SUB',s,4)
  'E':'S':'C':s -> returnP ('\ESC',s,4)
  'F':'S':s     -> returnP ('\FS', s,3)
  'G':'S':s     -> returnP ('\GS', s,3)
  'R':'S':s     -> returnP ('\RS', s,3)
  'U':'S':s     -> returnP ('\US', s,3)
  'S':'P':s     -> returnP ('\SP', s,3)
  'D':'E':'L':s -> returnP ('\DEL',s,4)

  _             -> parseError "Illegal escape sequence"


-- Stolen from Hugs's Prelude
parseInteger :: Integer -> String -> Integer
parseInteger radix ds =
	foldl1 (\n d -> n * radix + d) (map (toInteger . digitToInt) ds)

-- Production cntrl from section B.2

cntrl :: String -> P (Char,String,Int)
cntrl (c:s) | c >= '@' && c <= '_' = returnP (chr (ord c - ord '@'), s,2)
cntrl _                            = parseError "Illegal control character"

nestedComment cont y x bol s =
   case s of
      '-':'}':s -> cont y (x+2) bol s
      '{':'-':s -> nestedComment (nestedComment cont) y (x+2) bol s
      '\t':s    -> nestedComment cont y (nextTab x) bol s
      '\n':s    -> nestedComment cont (y+1) 1 True s
      c:s       -> nestedComment cont y (x+1) bol s
      []        -> error "Internal error: nestedComment"


docComment f cont s loc y x 
  = let (s', comment, y') = slurpExtraCommentLines s [] y in
    cont (f comment) s' loc y' x  -- continue with the newline char
    
slurpExtraCommentLines s lines y
  = case rest of
	'\n':nextline -> 
		case dropWhile nonNewlineSpace nextline of 
		  '-':'-':s -> slurpExtraCommentLines s 
					((line++"\n"):lines) (y+1)
		  _ -> (rest, finished, y)
	other -> (rest, finished, y)
  where
	(line, rest) = break (== '\n') s
	finished = concat (reverse (line:lines))

nonNewlineSpace c = isSpace c && c /= '\n'

docSection cont s loc y x
  = let (stars, rest') = break (/= '*') s
        (line,  rest) = break (== '\n') rest'
    in
    cont (DocSection (length stars) line) rest loc y x
\end{code}
