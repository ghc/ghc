
module CmdLexer ( Lexeme(..), Token(..), tokenise,
                  getLex, HasLineNo(..), HasTokNo(..), isVarChar ) 
where


import Char		( isAlpha, isDigit, isSpace )


---------------------------------------------------------------------
-- nano-lexer
data Lexeme
   = LString   String		-- "string"
   | LText     String		-- some_lump_of_text
   | LVar      String		-- $varname
   | LBool     Bool		-- True or False
   | L_Test			-- test
   | L_Exists			-- exists
   | L_When			-- when
   | L_Expect			-- expect
   | L_And			-- &&
   | L_Or			-- ||
   | L_Append			-- ++
   | L_Framefail		-- framefail
   | L_Pass 			-- pass
   | L_Fail			-- fail
   | L_Unknown			-- unknown
   | L_Skip			-- skip
   | L_Contains			-- contains
   | L_Lacks			-- lacks
   | L_Return			-- return
   | L_Eq			-- ==
   | L_NEq			-- /=
   | L_Assign			-- =
   | L_Otherwise		-- otherwise
   | L_Open			-- (
   | L_Close			-- )
   | L_LBrace			-- {
   | L_RBrace			-- }
   | L_Comma			-- ,
   | L_Bar			-- |
   | L_Include			-- include
   | L_If			-- if
   | L_Then			-- then
   | L_Else			-- else
   | L_Fi			-- fi
   | L_Def			-- def
   | L_Print			-- print
   | L_Run			-- run
   | L_Defined			-- defined
   | L_Contents			-- contents
     deriving (Eq, Show)

data Token
   = Tok Int Int Lexeme -- token #, line #, Lex
     deriving Show

getLex   (Tok tno lno lexeme) = lexeme

class HasLineNo a where
   getLineNo :: a -> Int
instance HasLineNo Token where
   getLineNo (Tok tno lno lex) = lno
instance HasLineNo a => HasLineNo [a] where
   getLineNo []     = 999999 	-- EOF presumably
   getLineNo (t:ts) = getLineNo t

class HasTokNo a where
   getTokNo :: a -> Int
instance HasTokNo Token where
   getTokNo (Tok tno lno lex) = tno
instance HasTokNo a => HasTokNo [a] where
   getTokNo []     = 999999 	-- EOF presumably
   getTokNo (t:ts) = getTokNo t

bomb = 0 :: Int

tokenise :: Int -> String -> [Token]
tokenise n toks
  = let un_numbered = tokenise_wrk n toks
        f tok_no (Tok _ lno lex) = Tok tok_no lno lex
    in  zipWith f [1..] un_numbered

-- do the biz, but don't insert token #s
tokenise_wrk :: Int -> String -> [Token]
tokenise_wrk n [] = []

tokenise_wrk n ('&':'&':cs) = (Tok bomb n L_And) : tokenise_wrk n cs
tokenise_wrk n ('|':'|':cs) = (Tok bomb n L_Or) : tokenise_wrk n cs
tokenise_wrk n ('+':'+':cs) = (Tok bomb n L_Append) : tokenise_wrk n cs
tokenise_wrk n ('=':'=':cs) = (Tok bomb n L_Eq) : tokenise_wrk n cs
tokenise_wrk n ('/':'=':cs) = (Tok bomb n L_NEq) : tokenise_wrk n cs

tokenise_wrk n (c:cs)
   | take 2 (c:cs) == "--"
   = tokenise_wrk n (dropWhile (/= '\n') (c:cs))
   | c == '\n'
   = tokenise_wrk (n+1) cs
   | isSpace c 
   = tokenise_wrk n cs
   | c == '$' 
   = let (vs, rest) = takeDrop isVarChar cs
     in  (Tok bomb n (LVar vs)) : tokenise_wrk n rest
   | c == '"'	-- "
   = let str  = takeLitChars cs
         rest = drop (length str) cs
     in (Tok bomb n (LString (escIfy str))) : tokenise_wrk n (drop 1 rest)
   | c == '('
   = (Tok bomb n L_Open) : tokenise_wrk n cs
   | c == ')'
   = (Tok bomb n L_Close) : tokenise_wrk n cs
   | c == '{'
   = (Tok bomb n L_LBrace) : tokenise_wrk n cs
   | c == '}'
   = (Tok bomb n L_RBrace) : tokenise_wrk n cs
   | c == ','
   = (Tok bomb n L_Comma) : tokenise_wrk n cs
   | c == '|'
   = (Tok bomb n L_Bar) : tokenise_wrk n cs
   | c == '='
   = (Tok bomb n L_Assign) : tokenise_wrk n cs
   | otherwise
   = let (str,rest) = takeDrop (`notElem` "(), \n\t") (c:cs)
         kw x = (Tok bomb n x) : tokenise_wrk n rest
     in  case str of
            "framefail"    -> kw L_Framefail
            "defined"      -> kw L_Defined
            "contents"     -> kw L_Contents
            "def"          -> kw L_Def
            "run"          -> kw L_Run
            "if"           -> kw L_If
            "then"         -> kw L_Then
            "else"         -> kw L_Else
            "fi"           -> kw L_Fi
            "print"        -> kw L_Print
            "test"         -> kw L_Test
            "exists"       -> kw L_Exists
            "when"         -> kw L_When
            "expect"       -> kw L_Expect
            "pass"         -> kw L_Pass
            "fail"         -> kw L_Fail
            "unknown"      -> kw L_Unknown
            "skip"         -> kw L_Skip
            "contains"     -> kw L_Contains
            "lacks"        -> kw L_Lacks
            "return"       -> kw L_Return
            "otherwise"    -> kw L_Otherwise
            "include"      -> kw L_Include
            "True"         -> kw (LBool True)
            "False"        -> kw (LBool False)
            other          -> kw (LText other)

takeDrop :: (Char -> Bool) -> String -> (String, String)
takeDrop p cs = let taken = takeWhile p cs
                in  (taken, drop (length taken) cs)

isVarChar c = isAlpha c || isDigit c || c `elem` "_-"

escIfy [] = []
escIfy ('\\':'\\':cs) = '\\':escIfy cs
escIfy ('\\':'n':cs)  = '\n':escIfy cs
escIfy ('\\':'"':cs)  = '"':escIfy cs
escIfy (c:cs) = c : escIfy cs

takeLitChars [] = []
takeLitChars ('\\':'"':cs) = '\\':'"':takeLitChars cs
takeLitChars ('"':cs) = []	-- "
takeLitChars (c:cs) = c : takeLitChars cs
