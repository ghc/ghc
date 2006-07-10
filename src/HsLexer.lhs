-----------------------------------------------------------------------------
-- $Id: HsLexer.lhs,v 1.18 2005/03/09 08:28:39 wolfgang Exp $
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
import HsSyn2

import Numeric	( readHex, readOct )
import Char
import List     ( isPrefixOf )
\end{code}

\begin{code}
data Token 
        = VarId String
	| IPVarId String
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

	| DocCommentNext  String	-- something beginning '-- |'
	| DocCommentPrev  String	-- something beginning '-- ^'
	| DocCommentNamed String	-- something beginning '-- $'
	| DocSection      Int String	-- a section heading
	| DocOptions      String	-- attributes '-- #'

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
 ( "mdo",       KW_Do ),       -- pretend mdo is do, for now.
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
 ( "hiding", 	KW_Hiding ),
 ( "stdcall",   KW_StdCall )
 ]

specialIds = [
  KW_As,
  KW_Unsafe,
  KW_Safe,
  KW_ThreadSafe,
  KW_Qualified,
  KW_Hiding,
  KW_Export,
  KW_StdCall,
  KW_CCall,
  KW_DotNet
  ]

isIdent, isSymbol, isWhite :: Char -> Bool
isIdent  c = isAlpha c || isDigit c || c == '\'' || c == '_'
isSymbol c = elem c ":!#$%&*+./<=>?@\\^|-~"
isWhite  c = elem c " \n\r\t\v\f"

isIdentInitial :: Char -> Bool
isIdentInitial ch = isLower ch || ch == '_'

tAB_LENGTH :: Int
tAB_LENGTH = 8

-- The source location, (y,x), is the coordinates of the previous token.
-- col is the current column in the source file.  If col is 0, we are
-- somewhere at the beginning of the line before the first token.

-- Setting col to 0 is used in two places: just after emitting a virtual
-- close brace due to layout, so that next time through we check whether
-- we also need to emit a semi-colon, and at the beginning of the file,
-- to kick off the lexer.


lexer :: (Token -> P a) -> P a
lexer cont input (SrcLoc _ x0 _) y0 col f =
        if col == 0
           then tab y0 x0  f True  input
           else tab y0 col f False input -- throw away old x
  where
   	-- move past whitespace and comments
        tab y x f _ [] = 
        	cont EOF [] (SrcLoc y x f) y col f
        tab y x f bol ('\t':s) =
        	tab y (nextTab x) f bol s
        tab y _ f _  ('\n':s) =
                newLine cont s y f

        tab y _ f True ('#':s)
            | "pragma GCC set_debug_pwd" `isPrefixOf` s
            = newLine cont (tail $ dropWhile (/= '\n') s) y f

	tab y x f True ('#':' ':s@(d:_))
	    | isDigit d = parseLinePragma tab y f s

	-- single-line comments
        tab y x f bol s@('-':'-':' ':c:_) | doc c = 
	 	is_a_token bol s y x f
        tab y _ f _ ('-':'-':s) | null s || not (isSymbol (head (dropWhile (== '-') s)))  =
        	newLine cont (drop 1 (dropWhile (/= '\n') s)) y f

	-- multi-line nested comments and pragmas
	tab y x f bol ('{':'-':'#':s) = pragma tab y (x+3) f bol s
	tab y x f bol s@('{':'-':c:_) | doc c = 
		is_a_token bol s y x f
	tab y x f bol s@('{':'-':' ':c:_) | doc c = 
		is_a_token bol s y x f
        tab y x f bol ('{':'-':s) = nestedComment (\y x -> tab y x f) y (x+2) bol s

        tab y x f bol (c:s)
        	| isWhite c = tab y (x+1) f bol s
        	| otherwise = is_a_token bol (c:s) y x f

 	is_a_token bol s y x f
	   | bol       = lexBOL   cont s (SrcLoc y x f) y x f
	   | otherwise = lexToken cont s (SrcLoc y x f) y x f

	newLine _ s y f =  tab (y+1) 1 f True s

	doc '|' = True
	doc '/' = True
	doc '^' = True
	doc '*' = True
	doc '$' = True
	doc '#' = True
	doc _ = False

nextTab :: Int -> Int
nextTab x = x + (tAB_LENGTH - (x-1) `mod` tAB_LENGTH)

-- When we are lexing the first token of a line, check whether we need to
-- insert virtual semicolons or close braces due to layout.

lexBOL :: (Token -> P a) -> P a
lexBOL cont s loc y x f context =
        if need_close_curly then 
                -- trace "layout: inserting '}'\n" $
        	-- Set col to 0, indicating that we're still at the
        	-- beginning of the line, in case we need a semi-colon too.
        	-- Also pop the context here, so that we don't insert
        	-- another close brace before the parser can pop it.
        	cont VRightCurly s loc y 0 f (tail context)
        else if need_semi_colon then
                --trace "layout: inserting ';'\n" $
        	cont SemiColon s loc y x f context
        else
        	lexToken cont s loc y x f context
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
--lexToken _    [] loc _ _ =
--    error $ "Internal error: empty input in lexToken at " ++ show loc
lexToken cont s0 loc y x f =
   -- trace ("lexer: y="++show y++" x="++show x++"\n") $ 
   case s0 of
        []            -> error $ "Internal error: empty input in lexToken at "
                                 ++ show loc
	-- First the doc comments
	'-':'-':' ':s -> do_doc s False
	'{':'-':' ':s -> do_doc s True
	'{':'-':s     -> do_doc s True

        -- Next the special symbols
        '(':'#':s -> forward 2 LeftUT s
	'(':s 	  -> forward 1 LeftParen s
	'#':')':s -> forward 2 RightUT s
        ')':s     -> forward 1 RightParen s
        ',':s     -> forward 1 Comma s
        ';':s     -> forward 1 SemiColon s
        '[':s     -> forward 1 LeftSquare s
        ']':s     -> forward 1 RightSquare s
        '`':s     -> forward 1 BackQuote s
        '{':s     -> \ctxt  -> forward 1 LeftCurly s (NoLayout : ctxt)
        '}':s     -> \ctxt0 -> case ctxt0 of
                               (_:ctxt) -> forward 1 RightCurly s ctxt
						-- pop context on '}'
                               []       -> error "Internal error: empty context in lexToken"

        '?':s:ss  
	  | isIdentInitial s -> lexToken ( \ (VarId x) -> cont (IPVarId x)) (s:ss) loc y x f
        '\'':s -> lexChar cont s loc y (x+1) f
        '\"':s{-"-} -> lexString cont s loc y (x+1) f

        '0':'x':c:s | isHexDigit c -> 
	   let (num, rest) = span isHexDigit (c:s)
	       [(i,_)] = readHex num
	   in
	   afterNum cont i rest loc y (x+length num) f
        '0':'o':c:s | isOctDigit c -> 
	   let (num, rest) = span isOctDigit (c:s)
	       [(i,_)] = readOct num
	   in
	   afterNum cont i rest loc y (x+length num) f

        c:s | isIdentInitial c ->
        	let 
        	    (idtail, rest) = slurpIdent s
        	    id0 = c:idtail
        	    l_id = 1 + length idtail
        	in
        	case lookup id0 reserved_ids of
        		Just keyword -> forward l_id keyword rest
        		Nothing      -> forward l_id (VarId id0) rest

          | isUpper c -> lexCon "" cont (c:s) loc y x f
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

          | isDigit c -> lexNum cont c s loc y x f

          | otherwise ->
        	parseError ("illegal character \'" ++ show c ++ "\'\n") 
        		  s loc y x f

  where forward n t str = cont t str loc y (x+n) f

	-- this is all terribly ugly, sorry :(
	do_doc ('|':s) nested  = multi nested DocCommentNext  cont s loc y x f
	do_doc ('/':s) nested  = multi nested DocCommentNext  cont s loc y x f
	do_doc ('^':s) nested  = multi nested DocCommentPrev  cont s loc y x f
	do_doc ('$':s) nested  = multi nested DocCommentNamed cont s loc y x f
	do_doc ('#':s) nested  = multi nested DocOptions      cont s loc y x f
	do_doc ('*':s) nested  = section 1 s
	  where section n ('*':s1) = section (n+1) s1
		section n s1 
		 | nested    = nestedDocComment  (DocSection n) cont s1 loc y x f
		 | otherwise = oneLineDocComment (DocSection n) cont s1 loc y x f
	do_doc _       _ = error "Internal error: HsLexer.do_doc"
        

multi :: Bool -> ([Char] -> b)
      -> (b -> [Char] -> c -> Int -> Int -> d)
      -> [Char] -> c -> Int -> Int -> d
multi True  = nestedDocComment
multi False = multiLineDocComment

afterNum :: Num a => (Token -> [Char] -> b -> c -> a -> d -> e)
	 -> Integer -> [Char] -> b -> c -> a -> d -> e
afterNum cont i ('#':s) loc y x f = cont (PrimInt i) s loc y (x+1) f
afterNum cont i s loc y x f = cont (IntTok i) s loc y x f

lexNum :: (Token -> [Char] -> a -> b -> Int -> c -> d)
       -> Char -> [Char] -> a -> b -> Int -> c -> d
lexNum cont c0 s0 loc y x fname = 
  let (num, after_num) = span isDigit (c0:s0)
  in
  case after_num of
    '.':c1:s1 | isDigit c1 ->
	let (frac,after_frac) = span isDigit s1
	in
	let float = num ++ '.':frac
	    (f, after_exp)
		 = case after_frac of
		    'E':s -> do_exponent s
		    'e':s -> do_exponent s
		    _     -> (float, after_frac)

	    do_exponent s2 =
		 case s2 of
		  '-':c:s | isDigit c -> 
			let (exp0,rest) = span isDigit (c:s) in
			(float ++ 'e':'-':exp0, rest)
		  '+':c:s | isDigit c -> 
			let (exp0,rest) = span isDigit (c:s) in
			(float ++ 'e':'+':exp0, rest)
		  c:s | isDigit c -> 
			let (exp0,rest) = span isDigit (c:s) in
			(float ++ 'e':exp0, rest)
		  _ -> (float, after_frac)

	    x' = x + length f

	in case after_exp of -- glasgow exts only
		'#':'#':s -> cont (PrimDouble f) s loc y x' fname
		'#':s     -> cont (PrimFloat f)  s loc y x' fname
		s         -> cont (FloatTok f)   s loc y x' fname

    _ -> afterNum cont (parseInteger 10 num) after_num loc y (x + length num) fname

		
-- GHC extension: allow trailing '#'s in an identifier. 
slurpIdent :: String -> (String, String)
slurpIdent s = slurp' s []
 where
  slurp' [] i = (reverse i, [])
  slurp' (c:cs) i 
    | isIdent c = slurp' cs (c:i)
    | c == '#'  = slurphashes cs (c:i)
  slurp' cs i = (reverse i, cs)

slurphashes :: String -> String -> (String, String)
slurphashes [] i = (reverse i, [])
slurphashes ('#':cs) i = slurphashes cs ('#':i)
slurphashes s i = (reverse i, s)

lexCon :: [Char] -> (Token -> String -> a -> b -> Int -> c -> d)
       -> String -> a -> b -> Int -> c -> d
lexCon qual cont s0 loc y x f =
  let
    forward n t s = cont t s loc y (x+n) f

    (con, rest) = slurpIdent s0
    l_con = length con

    just_a_conid 
	| null qual = forward l_con (ConId con) rest
	| otherwise = forward l_con (QConId (qual,con)) rest

    qual' | null qual = con
	  | otherwise = qual ++ '.':con
  in
  case rest of
    '.':c1:s1 
     | isIdentInitial c1 ->	-- qualified varid?
	let
	    (idtail, rest1) = slurpIdent s1
	    id0 = c1:idtail
	    l_id = 1 + length idtail
	in
	case lookup id0 reserved_ids of
	   -- cannot qualify a reserved word
	   Just id | id `notElem` specialIds  -> just_a_conid
	   _ -> forward (l_con+1+l_id) (QVarId (qual', id0)) rest1

     | isUpper c1 ->	-- qualified conid?
	lexCon qual' cont (c1:s1) loc y (x+l_con+1) f

     | isSymbol c1 ->	-- qualified symbol?
	let
	    (symtail, rest1) = span isSymbol s1
	    sym = c1 : symtail
	    l_sym = 1 + length symtail
	in
	case lookup sym reserved_ops of
	    -- cannot qualify a reserved operator
	    Just _  -> just_a_conid
	    Nothing -> 
		case c1 of
		  ':' -> forward (l_con+1+l_sym) (QConSym (qual', sym)) rest1
		  _   -> forward (l_con+1+l_sym) (QVarSym (qual', sym)) rest1

    _ -> just_a_conid -- not a qualified thing


lexChar :: (Token -> P a) -> P a
lexChar cont s0 loc0 y x f = case s0 of
                    '\\':s1 -> (escapeChar s1 `thenP` \(e,s,i) _ _ _ _ _ ->
                               charEnd e s loc0 y (x+i) f) s1 loc0 y x f
                    c:s     -> charEnd c s loc0 y (x+1) f
                    []      -> char_err [] loc0 y x f

  where charEnd c ('\'':'#':s) = \loc y0 x0 f0 -> cont (PrimChar c) s loc y0 (x0+2) f0
	charEnd c ('\'':s) = \loc y0 x0 f0 -> cont (Character c) s loc y0 (x0+1) f0
	charEnd c s = char_err s

	char_err s = parseError "Improperly terminated character constant" s

lexString :: (Token -> P a) -> P a
lexString cont s0 loc y0 x0 f0 = loop "" s0 x0 y0 f0
  where
     loop e s1 x y f = case s1 of
            '\\':'&':s  -> loop e s (x+2) y f
            '\\':c:s | isSpace c -> stringGap e s (x+2) y f
        	     | otherwise -> (escapeChar (c:s) `thenP` \(e',s2,i) _ _ _ _ ->
        		             loop (e':e) s2 (x+i) y) s loc y x f
            '\"':'#':s  -> cont (PrimString (reverse e)) s loc y (x+2) f
            '\"':s{-"-} -> cont (StringTok (reverse e)) s loc y (x+1) f
            c:s		-> loop (c:e) s (x+1) y f
            []          -> parseError "Improperly terminated string" s1 loc y x f

     stringGap e s1 x y = case s1 of
        	'\n':s -> stringGap e s 1 (y+1)
        	'\\':s -> loop e s (x+1) y
        	c:s | isSpace c -> stringGap e s (x+1) y
        	    | otherwise -> 
        	       parseError "Illegal character in string gap" s1 loc y x
                []     -> error "Internal error: stringGap"

-- ToDo: \o, \x, \<octal> things.

escapeChar :: String -> P (Char,String,Int)
escapeChar s0 = case s0 of

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

  '^':x@(_:_)   -> cntrl x
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


pragma :: (Int -> Int -> FilePath -> Bool -> [Char] -> b)
       -> Int -> Int -> FilePath -> Bool -> [Char] -> b
pragma cont y x f bol s0 =
   case span (==' ') s0 of
      (_, 'L':'I':'N':'E':' ':s) -> parseLinePragma cont y f s
      (_, 'l':'i':'n':'e':' ':s) -> parseLinePragma cont y f s
      (sp,s) -> nestedComment (\y x -> cont y x f) y (x+length sp) bol s

parseLinePragma :: (Int -> Int -> FilePath -> Bool -> [Char] -> b)
                -> Int -> FilePath -> [Char] -> b
parseLinePragma cont y fname s0 =
		cont y' 1 fname' True (drop 1 (dropWhile (/= '\n') s0))

  where  s1            = dropWhite s0
         (lineStr, s2) = span isDigit s1
         y'            = case reads lineStr of
	                   ((y',_):_) -> y'
			   _          -> y
         s3            = dropWhite s2
	 fnameStr      = takeWhile (\c -> c /= '"' && c/='\n') (tail s3)
         fname'        | null s3 || head s3 /= '"' = fname
                       -- try and get more sharing of file name strings
                       | fnameStr == fname         = fname
                       | otherwise                 = fnameStr
         dropWhite = dropWhile (\c -> c == ' ' || c == '\t')

nestedComment :: (Int -> Int -> Bool -> [Char] -> b)
	      -> Int -> Int -> Bool -> [Char] -> b
nestedComment cont y x bol s0 =
   case s0 of
      '-':'}':s -> cont y (x+2) bol s
      '{':'-':s -> nestedComment (nestedComment cont) y (x+2) bol s
      '\t':s    -> nestedComment cont y (nextTab x) bol s
      '\n':s    -> nestedComment cont (y+1) 1 True s
      _:s       -> nestedComment cont y (x+1) bol s
      []        -> error "Internal error: nestedComment"

nestedDocComment :: ([Char] -> b)
		 -> (b -> [Char] -> c -> Int -> Int -> d)
                 -> [Char] -> c -> Int -> Int -> d
nestedDocComment f0 cont0 s0 loc y0 x0 = go f0 cont0 "" y0 x0 s0
 where
  go f cont acc y1 x1 s1 = 
   case s1 of
      '-':'}':s -> cont (f (reverse acc)) s loc y1 (x1+2)
      '{':'-':s -> nestedComment (\y x _ s2 -> go f cont acc y x s2) 
			y1 (x1+2) False s
      '\t':s    -> go f cont ('\t':acc) y1 (nextTab x1) s
      '\n':s    -> go f cont ('\n':acc) (y1+1) 1 s
      c:s       -> go f cont (c:acc) y1 (x1+1) s
      []        -> error "Internal error: nestedComment"

oneLineDocComment :: ([Char] -> a)
		  -> (a -> [Char] -> b -> c -> d -> e)
		  -> [Char] -> b -> c -> d -> e
oneLineDocComment f cont s loc y x
  = cont (f line) rest loc y x -- continue with the newline char
  where (line, rest) = break (== '\n') s

multiLineDocComment :: Num a => ([Char] -> b)
		    -> (b -> [Char] -> c -> a -> d -> e)
		    -> [Char] -> c -> a -> d -> e
multiLineDocComment f cont s loc y x 
  = cont (f comment) s' loc y' x -- continue with the newline char
  where (s', comment, y') = slurpExtraCommentLines s [] y
    
slurpExtraCommentLines :: Num a => [Char] -> [[Char]] -> a 
                       -> ([Char], [Char], a)
slurpExtraCommentLines s0 lines0 y
  = case rest of
	'\n':nextline -> 
		case dropWhile nonNewlineSpace nextline of 
		  -- stop slurping if we see a string of more than two '-';
		  -- strings of dashes are useful as separators but we don't
		  -- want them in the doc.
		  '-':'-':c:s | c /= '-'
			 -> slurpExtraCommentLines (c:s)
				((line++"\n"):lines0) (y+1)
		  _ -> (rest, finished, y)
	_ -> (rest, finished, y)
  where
	(line, rest) = break (== '\n') s0
	finished = concat (reverse (line:lines0))

nonNewlineSpace :: Char -> Bool
nonNewlineSpace c = isSpace c && c /= '\n'
\end{code}
