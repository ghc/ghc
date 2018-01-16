
-- ==========================================================--
-- === Raw lexical analysis (tokenisation) of source      ===--
-- ===                                           Lexer.hs ===--
-- ==========================================================--

module Main where
import Data.Char -- 1.3
----------------------------------------------------------
-- Lexemes                                              --
----------------------------------------------------------

type Token = (Int, Int, Lex, String) -- (line, column, lexeme type, value)

data Lex = Lcon             -- constructor used as prefix:
                            -- normal prefix constructor,
                            -- or bracketed infix constructor

         | Lconop           -- constructor used as infix:
                            -- normal prefix constructor in backquotes,
                            -- or infix constructor (starting with ":")

         | Lvar             -- variable used as prefix:
                            -- normal prefix variable,
                            -- or bracketed infix var (operator)

         | Lvarop           -- variable used as infix:
                            -- normal prefix variable in backquotes,
                            -- or infix variable (operator)

         -- | Ltycon          -- constructor starting with A-Z
                              -- subcase of Lcon

         -- | Ltyvar          -- variable starting with a-z
                              -- subcase of Lvar

         | Lintlit          -- integer literal
         | Lcharlit         -- character literal
         | Lstringlit       -- string literal

         | Llbrace          --  {
         | Lrbrace          --  }
         | Lsemi            --  ;
         | Lequals          --  =
         | Lbar             --  |
         | Larrow           --  ->
         | Llparen          --  (
         | Lrparen          --  )
         | Lcomma           --  ,
         | Llbrack          --  [
         | Lrbrack          --  ]
         | Lunder           --  _
         | Lminus           --  -
         | Lslash           --  \

         | Lmodule
         | Linfixl
         | Linfixr
         | Linfix
         | Lext
         | Ldata
         | Lif
         | Lthen
         | Lelse
         | Llet
         | Lin
         | Lcase
         | Lof
         | Lwhere

         | Leof deriving (Eq, Show{-was:Text-})

{- 
   Lexing rules:

   case (
      if next is \,                                         -> Llparen
      if next is symbol, take symbols and expect closing )  -> Lvar
      if next is :, take tail-ident-chars, expect closing ) -> Lcon
      otherwise                                             -> Llparen

   case `
      if next A-Z, take tail-ident-chars, expect `          -> Lconop
      if next a-z, take tail-ident-chars, expect `          -> Lvarop
      otherwise                                             -> error

   case A-Z
      take tail-ident-chars                                 -> Lcon

   case a-z
      take tail-ident-chars                                 -> Lvar

   case 0-9
      take 0-9s                                             -> Lintlit

   case '
      expect a lit-char, then '                             -> charlit

   case "
      expect lit-chars, then "                              -> stringlit

   case {
      case -                                                -> run_comment
      otherwise                                             -> Llbrace

   case }                                                   -> Lrbrace

   case )                                                   -> Lrparen

   case [                                                   -> Llbrack
   case ]                                                   -> Lrbrack

   case ;                                                   -> Lsemi
   case ,                                                   -> Lcomma
   case _                                                   -> Lunder
   case -
      case -                                                -> line_comment
      case >                                                -> Larrow
      otherwise                                             -> Lminus

   case # in column 1: this is a preprocessor line

   case :!#$%&*+./<=>?@\^|~
      take symbols, then case resulting
         "="                                                -> Lequals
         "|"                                                -> Lbar
         "\"                                                -> Lslash
         otherwise
            if starts with :                                -> Lconop
            else                                            -> lvarop
-}



-- ==========================================================--
--
leLex :: Int -> Int -> String -> [Token]

leLex l n [] 
   = repeat (99997, 99997, Leof, "")

leLex l n ('(':[])
   = [(l, n, Llparen, ")")]

leLex l n ('(':c:cs)
   | c == ':'
   = case leChunk (n+1) leIsTailChar cs of
        (restSym, nn, restInput) -> case restInput of
           []        -> leFail l nn "  )  expected"
           (')':as)  -> (l, n, Lvar, c:restSym) : leLex l (nn+1) as
           (_:_)     -> leFail l nn "  )  expected"
   | c == '\\'
   = (l, n, Llparen, "(") : leLex l (n+1) (c:cs)
   | leIsSymbol c
   = case leChunk (n+1) leIsSymbol cs of
        (restSym, nn, restInput) -> case restInput of
           []        -> leFail l nn "  )  expected"
           (')':as)  -> (l, n, Lvar, c:restSym) : leLex l (nn+1) as
           (_:_)     -> leFail l nn "  )  expected"
   | otherwise
   = (l, n, Llparen, "(") : leLex l (n+1) (c:cs)

leLex l n ('`':c:cs)
   | isAlpha c
   = case leChunk (n+1) isAlpha cs of
        (restSym, nn, restInput) -> case restInput of
           []        -> leFail l nn "  `  expected"
           ('`':as)  -> (l, n, if isUpper c then Lconop else Lvarop, c:restSym) 
                        : leLex l (nn+1) as
           (_:_)     -> leFail l nn "  `  expected"
   | otherwise
   = leFail l n "Bad infix operator"

leLex l n ('"':cs)
   = case leTakeLitChars True l (n+1) cs of
        (restSym, nn, restInput) -> case restInput of
           []        -> leFail l nn "  \"  expected"
           ('"':as)  -> (l, n, Lstringlit, restSym) : leLex l (nn+1) as
           (_:_)     -> leFail l nn "  \"  expected"

leLex l n ('\'':cs)
   = case leTakeLitChars False l (n+1) cs of
        (restSym, nn, restInput) -> case restInput of
           []        -> leFail l nn "  '  expected"
           ('\'':as) -> case restSym of
                           [_] -> (l, n, Lcharlit, restSym) : leLex l (nn+1) as
                           _   -> leFail l (n+1) "Bad character literal"
           (_:_)     -> leFail l nn "  '  expected"

leLex l n ('}':cs)
   = (l, n, Lrbrace, "}") : leLex l (n+1) cs

leLex l n (')':cs)
   = (l, n, Lrparen, ")") : leLex l (n+1) cs

leLex l n ('[':cs)
   = (l, n, Llbrack, "[") : leLex l (n+1) cs

leLex l n (']':cs)
   = (l, n, Lrbrack, "]") : leLex l (n+1) cs

leLex l n (';':cs)
   = (l, n, Lsemi, ";") : leLex l (n+1) cs

leLex l n (',':cs)
   = (l, n, Lcomma, ",") : leLex l (n+1) cs

leLex l n ('_':cs)
   = (l, n, Lunder, "_") : leLex l (n+1) cs

leLex l n ('{':cs)
   = case cs of
        []         -> [(l, n, Llbrace, "}")]
        ('-':cs2)  -> leLexRComment l (n+2) cs2
        (_:_)      -> (l, n, Llbrace, "}") : leLex l (n+1) cs

leLex l n ('-':cs)
   = case cs of
        []         -> [(l, n, Lminus, "-")]
        ('-':cs2)  -> leLexLComment l (n+2) cs2
        ('>':cs3)  -> (l, n, Larrow, "->") : leLex l (n+2) cs3
        ('}':cs3)  -> leFail l n "Misplaced -}"
        (_:_)      -> (l, n, Lminus, "-") : leLex l (n+1) cs

leLex l n (' ':cs) 
   = leLex l (n+1) cs

leLex l n ('\n':cs)
   = leLex (l+1) 1 cs

leLex l n ('\t':cs)
   = leLex l (n - (n `mod` 8) + 9) cs

leLex l n (c:cs)
   = if   c == '#'
     then if n == 1
          then
          {- This is a CPP line number thingy -}
          let lineNoText  = takeWhile isDigit (tail cs)
              lineNo      = leStringToInt lineNoText
              nextLine    = drop 1 (dropWhile ((/=) '\n') cs)
          in
              leLex lineNo 1 nextLine
          else
          {- it's a symbol starting with # -}
          case leChunk (n+1) leIsSymbol cs of
             (restSym, nn, restText) -> (l, n, Lvarop, c:restSym) :
                                        leLex l nn restText
     else
     if   isAlpha c
     then case leChunk (n+1) leIsTailChar cs of
             (restSym, nn, restText) -> (l, n, if   isUpper c 
                                               then Lcon 
                                               else Lvar, c:restSym) :
                                        leLex l nn restText 
     else
     if   isDigit c
     then case leChunk (n+1) isDigit cs of
             (restSym, nn, restText) -> (l, n, Lintlit, c:restSym) :
                                        leLex l nn restText 
     else
     if   leIsSymbol c
     then case leChunk (n+1) leIsSymbol cs of
             (restSym, nn, restText) -> (l, n, if   c == ':' 
                                               then Lconop 
                                               else Lvarop, c:restSym) :
                                        leLex l nn restText 
     else
     leFail l n ("Illegal character  " ++ [c])


-- ==========================================================--
--
leChunk :: Int -> (Char -> Bool) -> String -> (String, Int, String)

leChunk n proper []	
  = ([], n, [])

leChunk n proper (c:cs)
  | proper c		
  = case leChunk (n+1) proper cs of
       (restId, col, restInput) -> (c:restId, col, restInput)
  | otherwise
  = ([], n, c:cs)


-- ==========================================================--
--
leTakeLitChars :: Bool -> Int -> Int -> String -> (String, Int, String)

leTakeLitChars d l n []
  = leFail l n "End of file inside literal"

leTakeLitChars d l n ('\\':'\\':cs)
  = case leTakeLitChars d l (n+2) cs of
       (rest, col, left) -> ('\\':rest, col, left)

leTakeLitChars d l n ('\\':'n':cs)
  = case leTakeLitChars d l (n+2) cs of
       (rest, col, left) -> ('\n':rest, col, left)

leTakeLitChars d l n ('\\':'t':cs)
  = case leTakeLitChars d l (n+2) cs of
       (rest, col, left) -> ('\t':rest, col, left)

leTakeLitChars d l n ('\\':'"':cs)
  = case leTakeLitChars d l (n+2) cs of
       (rest, col, left) -> ('"':rest, col, left)

leTakeLitChars d l n ('\\':'\'':cs)
  = case leTakeLitChars d l (n+2) cs of
       (rest, col, left) -> ('\'':rest, col, left)

leTakeLitChars d l n ('"':cs)
  | d      = ([], n, ('"':cs))
  | not d  = case leTakeLitChars d l (n+1) cs of
                (rest, col, left) -> ('"':rest, col, left)

leTakeLitChars d l n ('\'':cs)
  | not d  = ([], n, ('\'':cs))
  | d      = case leTakeLitChars d l (n+1) cs of
                (rest, col, left) -> ('\'':rest, col, left)

leTakeLitChars d l n ('\n':cs)
  = leFail l n "Literal exceeds line"

leTakeLitChars d l n ('\t':cs)
  = leFail l n "Literal contains tab"

leTakeLitChars d l n (c:cs)
  = case leTakeLitChars d l (n+1) cs of
       (rest, col, left) -> (c:rest, col, left)


-- ==========================================================--
--
leLexLComment :: Int -> Int -> String -> [Token]

leLexLComment l n cs
   = leLex (l+1) 1 (drop 1 (dropWhile ((/=) '\n') cs))


-- ==========================================================--
--
leLexRComment :: Int -> Int -> String -> [Token]

leLexRComment l n [] 
   = leFail l n "End of file inside {- ... -} comment"

leLexRComment l n ('-':'}':cs)
   = leLex l (n+2) cs

leLexRComment l n ('\n':cs)
   = leLexRComment (l+1) 1 cs

leLexRComment l n ('\t':cs)
   = leLexRComment l (n - (n `mod` 8) + 9) cs

leLexRComment l n (c:cs)
   = leLexRComment l (n+1) cs


-- ==========================================================--
--
leIsSymbol :: Char -> Bool

leIsSymbol c = c `elem` leSymbols

leSymbols = ":!#$%&*+./<=>?\\@^|~"


-- ==========================================================--
--
leIsTailChar :: Char -> Bool

leIsTailChar c 
   = isLower c || 
     isUpper c || 
     isDigit c || 
     c == '\'' || 
     c == '_'  ||
     c == '\''


-- ==========================================================--
--
leIsLitChar :: Char -> Bool

leIsLitChar c
   = c /= '\n' &&
     c /= '\t' &&
     c /= '\'' &&
     c /= '"'


-- ==========================================================--
--
leStringToInt :: String -> Int

leStringToInt
   = let s2i []      = 0
         s2i (d:ds)  = (fromEnum d - fromEnum '0') + 10 *s2i ds
     in s2i . reverse


-- ==========================================================--
--
leFail l n m
  = faiL ("Lexical error, line " ++ show l ++ ", col " ++ show n ++ 
          ":\n   " ++ m )

faiL m = error ( "\n\n" ++ m ++ "\n" )

-- ==========================================================--
-- === end                                       Lexer.hs ===--
-- ==========================================================--

-- ==========================================================--
-- === Keyword spotting, and offside rule implementation  ===--
-- ===                                          Layout.hs ===--
-- ==========================================================--

--module Layout

-- ==========================================================--
--
laKeyword :: Token -> Token

laKeyword (l, n, what, text) 
   = let
        f Lvarop "="      = Lequals
        f Lvarop "|"      = Lbar
        f Lvarop "\\"     = Lslash

        f Lvar "module"   = Lmodule
        f Lvar "infix"    = Linfix
        f Lvar "infixl"   = Linfixl
        f Lvar "infixr"   = Linfixr
        f Lvar "ext"      = Lext
        f Lvar "data"     = Ldata
        f Lvar "if"       = Lif
        f Lvar "then"     = Lthen
        f Lvar "else"     = Lelse
        f Lvar "let"      = Llet
        f Lvar "in"       = Lin
        f Lvar "case"     = Lcase
        f Lvar "of"       = Lof
        f Lvar "where"    = Lwhere

        f item words      = item
        
     in
         (l, n, f what text, text)


-- ==========================================================--
--
laLayout :: Int -> [Int] -> [Token] -> [Token]

laLayout l s []
   = laRbrace (length s - 1) 99999 99999

laLayout l s (t1:[])
   = t1 : laRbrace (length s - 1) 99998 99998

laLayout l (s:ss) (t1@(l1, n1, w1, c1) :
                   t2@(l2, n2, w2, c2) : ts)

   | w1 `elem` [Lof, Llet, Lwhere] && w2 /= Llbrace
   = t1 :
     (l1, n1, Llbrace, "{") :
     t2 :
     laLayout l2 (n2:s:ss) ts 

   | l1 == l
   = t1 :
     laLayout l (s:ss) (t2:ts)

   | n1 > s
   = t1 :
     laLayout l1 (s:ss) (t2:ts)

   | n1 == s
   = (l1, n1, Lsemi, ";") :
     t1 :
     laLayout l1 (s:ss) (t2:ts)

   | n1 < s
   = (l1, n1, Lrbrace, "}") :
     laLayout l ss (t1:t2:ts)


-- ==========================================================--
--
laRbrace c l n 
   = take c (repeat (l, n, Lrbrace, "}"))

-- ==========================================================--
--
laMain :: String -> [Token]

laMain
   = laLayout 1 [0] . map laKeyword . leLex 1 1


-- ==========================================================--
-- === end                                      Layout.hs ===--
-- ==========================================================--

-- ==========================================================--
-- === Abstract syntax for modules                        ===--
-- ===                                       AbsSyntax.hs ===--
-- ==========================================================--

--module AbsSyntax where

--1.3:data Maybe a = Nothing 
--             | Just a

type AList a b = [(a, b)]

type Id = String

data Module 
   = MkModule Id [TopDecl]
             deriving (Show{-was:Text-})

data FixityDecl
   = MkFixDecl Id (Fixity, Int)
             deriving (Show{-was:Text-})

data DataDecl
   = MkDataDecl Id ([Id], [ConstrAltDecl])
             deriving (Show{-was:Text-})

data TopDecl
   = MkTopF FixityDecl
   | MkTopD DataDecl
   | MkTopV ValBind
             deriving (Show{-was:Text-})

data Fixity
   = InfixL
   | InfixR
   | InfixN
             deriving (Eq,Show{-was:Text-})

type ConstrAltDecl
   = (Id, [TypeExpr])

data TypeExpr = TypeVar    Id
              | TypeArr    TypeExpr TypeExpr
              | TypeCon    Id [TypeExpr]
              | TypeList   TypeExpr
              | TypeTuple  [TypeExpr]
             deriving (Show{-was:Text-})

data ValBind
   = MkValBind Int Lhs Expr
             deriving (Show{-was:Text-})

data Lhs
   = LhsPat Pat
   | LhsVar Id [Pat]
             deriving (Show{-was:Text-})

data Pat 
   = PatVar Id
   | PatCon Id [Pat]
   | PatWild
   | PatList   [Pat]
   | PatTuple  [Pat]
             deriving (Show{-was:Text-})

data Expr
   = ExprVar      Id
   | ExprCon      Id
   | ExprApp      Expr Expr
   | ExprLam      [Pat] Expr
   | ExprCase     Expr [ExprCaseAlt]
   | ExprLetrec   [ValBind] Expr
   | ExprWhere    Expr [ValBind]
   | ExprGuards   [(Expr, Expr)]
   | ExprLiteral  Literal
   | ExprList     [Expr]
   | ExprTuple    [Expr]
   | ExprIf       Expr Expr Expr
   | ExprBar
   | ExprFail
             deriving (Show{-was:Text-})

data ExprCaseAlt
   = MkExprCaseAlt Pat Expr
             deriving (Show{-was:Text-})

data Literal
   = LiteralInt     Int
   | LiteralChar    Char
   | LiteralString  String
             deriving (Show{-was:Text-})

-- ==========================================================--
-- === end                                   AbsSyntax.hs ===--
-- ==========================================================--

-- ==========================================================--
-- === Parser generics                                    ===--
-- ===                                   ParserGeneric.hs ===--
-- ==========================================================--

--module ParserGeneric

type PEnv = AList String (Fixity, Int)

data PResult a = POk    PEnv [Token] a
               | PFail  Token

type Parser a = PEnv -> [Token] -> PResult a

type PEntry = (Bool, Expr, Id)

-- ==========================================================--
--
pgItem :: Lex -> Parser String

pgItem x env [] = PFail pgEOF

pgItem x env ((l, n, w, t):toks)
   | x == w     = POk env toks t
   | otherwise  = PFail (l, n, w, t)


-- ==========================================================--
--
pgAlts :: [Parser a] -> Parser a

pgAlts ps env toks
   = let
        useAlts [] bestErrTok 
           = PFail bestErrTok
        useAlts (p:ps) bestErrTok
           = case p env toks of
                PFail someErrTok -> useAlts ps (further someErrTok bestErrTok)
                successful_parse -> successful_parse
        further x1@(l1, n1, w1, t1) x2@(l2, n2, w2, t2)
           =      if l2 > l1 then x2
             else if l1 > l2 then x1
             else if n1 > n2 then x1
             else x2
     in
        useAlts ps (head (toks ++ [pgEOF])) 


-- ==========================================================--
--
pgThen2 :: (a -> b -> c) -> 
           Parser a -> 
           Parser b -> 
           Parser c

pgThen2 combine p1 p2 env toks
   = case p1 env toks of
     {
       PFail tok1 
         -> PFail tok1 ;
       POk env1 toks1 item1 
         -> case p2 env1 toks1 of
            {
              PFail tok2 
                -> PFail tok2 ;
              POk env2 toks2 item2
                -> POk env2 toks2 (combine item1 item2)
            }
     }


-- ==========================================================--
--
pgThen3 :: (a -> b -> c -> d) -> 
           Parser a -> 
           Parser b -> 
           Parser c -> 
           Parser d

pgThen3 combine p1 p2 p3 env toks
   = case p1 env toks of
     {
       PFail tok1 
         -> PFail tok1 ;
       POk env1 toks1 item1 
         -> case p2 env1 toks1 of
            {
              PFail tok2 
                -> PFail tok2 ;
              POk env2 toks2 item2
                -> case p3 env2 toks2 of
                   {
                     PFail tok3
                       -> PFail tok3 ;
                     POk env3 toks3 item3
                       -> POk env3 toks3 (combine item1 item2 item3)
                   }
            }
     }


-- ==========================================================--
--
pgThen4 :: (a -> b -> c -> d -> e) -> 
           Parser a -> 
           Parser b -> 
           Parser c -> 
           Parser d ->
           Parser e

pgThen4 combine p1 p2 p3 p4 env toks
   = case p1 env toks of
     {
       PFail tok1 
         -> PFail tok1 ;
       POk env1 toks1 item1 
         -> case p2 env1 toks1 of
            {
              PFail tok2 
                -> PFail tok2 ;
              POk env2 toks2 item2
                -> case p3 env2 toks2 of
                   {
                     PFail tok3
                       -> PFail tok3 ;
                     POk env3 toks3 item3
                       -> case p4 env3 toks3 of
                          {
                            PFail tok4 
                              -> PFail tok4 ;
                            POk env4 toks4 item4
                              -> POk env4 toks4 (combine item1 item2 item3 item4)
                          }
                   }
            }
     }


-- ==========================================================--
--
pgZeroOrMore :: Parser a -> Parser [a]

pgZeroOrMore p env toks
   = case p env toks of
     {
       PFail tok1 
         -> POk env toks [] ;
       POk env1 toks1 item1 
         -> case pgZeroOrMore p env1 toks1 of
            {
              PFail tok2 
                -> POk env1 toks1 [item1] ;
              POk env2 toks2 item2_list
                -> POk env2 toks2 (item1 : item2_list)
            }
     }
         

-- ==========================================================--
--
pgOneOrMore :: Parser a -> Parser [a]

pgOneOrMore p
   = pgThen2 (:) p (pgZeroOrMore p)


-- ==========================================================--
--
pgApply :: (a -> b) -> Parser a -> Parser b

pgApply f p env toks
   = case p env toks of
     {
       PFail tok1
         -> PFail tok1 ;
       POk env1 toks1 item1
         -> POk env1 toks1 (f item1)
     }


-- ==========================================================--
--
pgTwoOrMoreWithSep :: Parser a -> Parser b -> Parser [a]

pgTwoOrMoreWithSep p psep
   = pgThen4
        (\i1 s1 i2 rest -> i1:i2:rest)
        p
        psep
        p 
        (pgZeroOrMore (pgThen2 (\sep x -> x) psep p))


-- ==========================================================--
--
pgOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]

pgOneOrMoreWithSep p psep
   = pgThen2 (:) p (pgZeroOrMore (pgThen2 (\sep x -> x) psep p))


-- ==========================================================--
--
pgZeroOrMoreWithSep :: Parser a -> Parser b -> Parser [a]

pgZeroOrMoreWithSep p psep
   = pgAlts
     [
        pgOneOrMoreWithSep p psep,
        pgApply (\x -> x:[]) p,
        pgEmpty []
     ]


-- ==========================================================--
--
pgOptional :: Parser a -> Parser (Maybe a)

pgOptional p env toks
   = case p env toks of
     {
       PFail tok1
         -> POk env toks Nothing ;
       POk env2 toks2 item2
         -> POk env2 toks2 (Just item2)
     }


-- ==========================================================--
--
pgGetLineNumber :: Parser a -> Parser (Int, a)

pgGetLineNumber p env toks
   = let 
         lineNo = case (head (toks ++ [pgEOF])) of (l, n, w, t) -> l
     in
         case p env toks of
         {
           PFail tok1
             -> PFail tok1 ;
           POk env2 toks2 item2
             -> POk env2 toks2 (lineNo, item2)
         }


-- ==========================================================--
--
pgEmpty :: a -> Parser a

pgEmpty item env toks
   = POk env toks item


-- ==========================================================--
--
pgEOF :: Token

pgEOF = (88888, 88888, Lvar, "*** Unexpected end of source! ***")


-- ============================================================--
-- === Some kludgey stuff for implementing the offside rule ===--
-- ============================================================--

-- ==========================================================--
--
pgEatEnd :: Parser ()

pgEatEnd env [] 
   = POk env [] ()

pgEatEnd env (tok@(l, n, w, t):toks)
   | w == Lsemi || w == Lrbrace   = POk env toks ()
   | otherwise                    = POk env (tok:toks) ()


-- ==========================================================--
--
pgDeclList :: Parser a -> Parser [a]

pgDeclList p
   = pgThen3 (\a b c -> b) (pgItem Llbrace) 
                           (pgOneOrMoreWithSep p (pgItem Lsemi))
                           pgEatEnd


-- ==========================================================--
-- === end                               ParserGeneric.hs ===--
-- ==========================================================--

-- ==========================================================--
-- === The parser.                                        ===--
-- ===                                          Parser.hs ===--
-- ==========================================================--

--module Parser where

{- FIX THESE UP -}
utLookupDef env k def
   = head ( [ vv | (kk,vv) <- env, kk == k] ++ [def] )
panic = error
{- END FIXUPS -}

paLiteral :: Parser Literal
paLiteral
   = pgAlts 
     [
        pgApply (LiteralInt . leStringToInt) (pgItem Lintlit),
        pgApply (LiteralChar . head)         (pgItem Lcharlit),
        pgApply LiteralString              (pgItem Lstringlit)
     ]

paExpr
   = pgAlts 
     [
        paCaseExpr, 
        paLetExpr, 
        paLamExpr,
        paIfExpr,
        paUnaryMinusExpr,
        hsDoExpr []
     ]

paUnaryMinusExpr
   = pgThen2
        (\minus (_, aexpr, _) -> 
             ExprApp (ExprApp (ExprVar "-") (ExprLiteral (LiteralInt 0))) aexpr)
        paMinus
        paAExpr

paCaseExpr
   = pgThen4
        (\casee expr off alts -> ExprCase expr alts)
        (pgItem Lcase)
        paExpr
        (pgItem Lof)
        (pgDeclList paAlt)

paAlt
   = pgAlts
     [
        pgThen4
           (\pat arrow expr wheres 
                -> MkExprCaseAlt pat (pa_MakeWhereExpr expr wheres))
           paPat
           (pgItem Larrow)
           paExpr
           (pgOptional paWhereClause),
        pgThen3
           (\pat agrdrhss wheres
                -> MkExprCaseAlt pat
                      (pa_MakeWhereExpr (ExprGuards agrdrhss) wheres))
           paPat
           (pgOneOrMore paGalt)
           (pgOptional paWhereClause)
     ]

paGalt
   = pgThen4
        (\bar guard arrow expr -> (guard, expr))
        (pgItem Lbar)
        paExpr
        (pgItem Larrow)
        paExpr

paLamExpr
   = pgThen4
        (\lam patterns arrow rhs -> ExprLam patterns rhs)
        (pgItem Lslash)
        (pgZeroOrMore paAPat)
        (pgItem Larrow)
        paExpr

paLetExpr
   = pgThen4
        (\lett decls inn rhs -> ExprLetrec decls rhs)
        (pgItem Llet)
        paValdefs
        (pgItem Lin)
        paExpr

paValdefs 
   = pgApply pa_MergeValdefs (pgDeclList paValdef)

pa_MergeValdefs 
   = id

paLhs
   = pgAlts
     [
        pgThen2 (\v ps -> LhsVar v ps) paVar (pgOneOrMore paPat),
        pgApply LhsPat paPat
     ]

paValdef
   = pgAlts
     [
        pgThen4
           (\(line, lhs) eq rhs wheres 
                -> MkValBind line lhs (pa_MakeWhereExpr rhs wheres))
           (pgGetLineNumber paLhs)
           (pgItem Lequals)
           paExpr
           (pgOptional paWhereClause),
        pgThen3
           (\(line, lhs) grdrhss wheres 
                -> MkValBind line lhs 
                      (pa_MakeWhereExpr (ExprGuards grdrhss) wheres))
           (pgGetLineNumber paLhs)
           (pgOneOrMore paGrhs)
           (pgOptional paWhereClause)
     ]

pa_MakeWhereExpr expr Nothing 
   = expr
pa_MakeWhereExpr expr (Just whereClauses) 
   = ExprWhere expr whereClauses

paWhereClause
   = pgThen2 (\x y -> y) (pgItem Lwhere) paValdefs
paGrhs
   = pgThen4
        (\bar guard equals expr -> (guard, expr))
        (pgItem Lbar)
        paExpr
        (pgItem Lequals)
        paExpr
        

paAPat
   = pgAlts
     [
        pgApply PatVar paVar,
        pgApply (\id -> PatCon id []) paCon,
        pgApply (const PatWild) (pgItem Lunder),
        pgApply PatTuple
                (pgThen3 (\l es r -> es)
                         (pgItem Llparen) 
                         (pgTwoOrMoreWithSep paPat (pgItem Lcomma))
                         (pgItem Lrparen)),
        pgApply PatList
                (pgThen3 (\l es r -> es)
                         (pgItem Llbrack) 
                         (pgZeroOrMoreWithSep paPat (pgItem Lcomma))
                         (pgItem Lrbrack)),
        pgThen3 (\l p r -> p)
                (pgItem Llparen)
                paPat
                (pgItem Lrparen)
     ]

paPat
   = pgAlts
     [
        pgThen2 (\c ps -> PatCon c ps)
                paCon
                (pgOneOrMore paAPat),
        pgThen3 (\ap c pa -> PatCon c [ap,pa])
                paAPat
                paConop
                paPat,
        paAPat
     ]


paIfExpr
 = pgThen4
      (\iff c thenn (t,f) -> ExprIf c t f)
      (pgItem Lif)
      paExpr
      (pgItem Lthen)
      (pgThen3
         (\t elsee f -> (t,f))
         paExpr
         (pgItem Lelse)
         paExpr
      )

paAExpr
 = pgApply (\x -> (False, x, []))
   (pgAlts 
    [
       pgApply ExprVar paVar,
       pgApply ExprCon paCon,
       pgApply ExprLiteral paLiteral,
       pgApply ExprList paListExpr,
       pgApply ExprTuple paTupleExpr,
       pgThen3 (\l e r -> e) (pgItem Llparen) paExpr (pgItem Lrparen)
    ]
   )

paListExpr
   = pgThen3 (\l es r -> es) 
             (pgItem Llbrack) 
             (pgZeroOrMoreWithSep paExpr (pgItem Lcomma))
             (pgItem Lrbrack)

paTupleExpr
   = pgThen3 (\l es r -> es) 
             (pgItem Llparen) 
             (pgTwoOrMoreWithSep paExpr (pgItem Lcomma))
             (pgItem Lrparen)

paVar = pgItem Lvar
paCon = pgItem Lcon
paVarop = pgItem Lvarop
paConop = pgItem Lconop
paMinus = pgItem Lminus

paOp
 = pgAlts [
            pgApply (\x -> (True, ExprVar x, x)) paVarop,
            pgApply (\x -> (True, ExprCon x, x)) paConop,
            pgApply (\x -> (True, ExprVar x, x)) paMinus
          ]

paDataDecl
   = pgThen2
        (\dataa useful -> useful)
        (pgItem Ldata)
        paDataDecl_main

paDataDecl_main
   = pgThen4
        (\name params eq drhs -> MkDataDecl name (params, drhs))
        paCon
        (pgZeroOrMore paVar)
        (pgItem Lequals)
        (pgOneOrMoreWithSep paConstrs (pgItem Lbar))

paConstrs
   = pgThen2
        (\con texprs -> (con, texprs))
        paCon
        (pgZeroOrMore paAType)

paType 
   = pgAlts
     [
        pgThen3 
           (\atype arrow typee -> TypeArr atype typee)
           paAType
           (pgItem Larrow)
           paType,
        pgThen2
           TypeCon
           paCon
           (pgOneOrMore paAType),
        paAType
     ]

paAType
   = pgAlts
     [
        pgApply TypeVar paVar,
        pgApply (\tycon -> TypeCon tycon []) paCon,
        pgThen3
           (\l t r -> t)
           (pgItem Llparen)
           paType
           (pgItem Lrparen),
        pgThen3
           (\l t r -> TypeList t)
           (pgItem Llbrack)
           paType
           (pgItem Lrbrack),
        pgThen3
           (\l t r -> TypeTuple t)
           (pgItem Llparen)
           (pgTwoOrMoreWithSep paType (pgItem Lcomma))
           (pgItem Lrparen)
     ]

paInfixDecl env toks
  = let dump (ExprVar v) = v
        dump (ExprCon c) = c
    in
    pa_UpdateFixityEnv 
       (pgThen3
          (\assoc prio name -> MkFixDecl name (assoc, prio))
          paInfixWord
          (pgApply leStringToInt (pgItem Lintlit)) 
          (pgApply (\(_, op, _) -> dump op) paOp)
          env 
          toks 
       )

paInfixWord
  = pgAlts
    [
       pgApply (const InfixL) (pgItem Linfixl),
       pgApply (const InfixR) (pgItem Linfixr),
       pgApply (const InfixN) (pgItem Linfix)
    ]

pa_UpdateFixityEnv (PFail tok) 
   = PFail tok

pa_UpdateFixityEnv (POk env toks (MkFixDecl name assoc_prio))
   = let 
         new_env = (name, assoc_prio) : env
     in
         POk new_env toks (MkFixDecl name assoc_prio)

paTopDecl
   = pgAlts
     [
        pgApply MkTopF paInfixDecl,
        pgApply MkTopD paDataDecl,
        pgApply MkTopV paValdef
     ]

paModule
   = pgThen4
        (\modyule name wheree topdecls -> MkModule name topdecls)
        (pgItem Lmodule)
        paCon
        (pgItem Lwhere)
        (pgDeclList paTopDecl)
   
parser_test toks
   = let parser_to_test
            = --paPat
              --paExpr
              --paValdef
              --pgZeroOrMore paInfixDecl
              --paDataDecl
              --paType
              paModule
              --pgTwoOrMoreWithSep (pgItem Lsemi) (pgItem Lcomma)
              
     in
         parser_to_test hsPrecTable toks

-- ==============================================--
-- === The Operator-Precedence parser (yuck!) ===--
-- ==============================================--

--
-- ==========================================================--
--
hsAExprOrOp 
 = pgAlts [paAExpr, paOp]

hsDoExpr :: [PEntry] -> Parser Expr
-- [PaEntry] is a stack of operators and atomic expressions
-- hsDoExpr uses a parser (hsAexpOrOp :: Parsr PaEntry) for atomic
-- expressions or operators

hsDoExpr stack env toks = 
  let
     (validIn, restIn, parseIn, err)
        = case hsAExprOrOp env toks of
             POk env1 toks1 item1
                -> (True, toks1, item1, panic "hsDoExpr(1)")
             PFail err
                -> (False, panic "hsDoExpr(2)", panic "hsDoExpr(3)", err)
     (opIn, valueIn, nameIn)
        = parseIn
     (assocIn, priorIn)
        = utLookupDef env nameIn (InfixL, 9)
     shift
        = hsDoExpr (parseIn:stack) env restIn
  in 
     case stack of
        s1:s2:s3:ss
           | validIn && opS2 && opIn && priorS2 > priorIn
              -> reduce
           | validIn && opS2 && opIn && priorS2 == priorIn
              -> if assocS2 == InfixL && 
                    assocIn == InfixL 
                 then reduce
	         else 
                 if assocS2 == InfixR && 
                    assocIn == InfixR 
                 then shift
	         else PFail (head toks) -- Because of ambiguousness 
           | not validIn && opS2
              -> reduce
             where
               (opS1, valueS1, nameS1) = s1
               (opS2, valueS2, nameS2) = s2
               (opS3, valueS3, nameS3) = s3
               (assocS2, priorS2) = utLookupDef env nameS2 (InfixL, 9)
               reduce = hsDoExpr ((False, ExprApp (ExprApp valueS2 valueS3) 
                                                  valueS1, [])
                                  : ss) env toks
        s1:s2:ss
           | validIn && (opS1 || opS2) -> shift
           | otherwise -> reduce
             where
                (opS1, valueS1, nameS1) = s1
                (opS2, valueS2, nameS2) = s2
                reduce = hsDoExpr ((False, ExprApp valueS2 valueS1, []) : ss) 
                                  env toks
        (s1:[])
           | validIn -> shift
           | otherwise -> POk env toks valueS1
             where
                (opS1, valueS1, nameS1) = s1
        []
           | validIn -> shift
           | otherwise -> PFail err

-- ==========================================================--
-- === end                                      Parser.hs ===--
-- ==========================================================--

hsPrecTable :: PEnv
hsPrecTable = [
  ("-",		(InfixL, 6)),
  ("+",		(InfixL, 6)),
  ("*",		(InfixL, 7)),
  ("div",	(InfixN, 7)),
  ("mod", 	(InfixN, 7)),

  ("<",		(InfixN, 4)),
  ("<=",	(InfixN, 4)),
  ("==",	(InfixN, 4)),
  ("/=",	(InfixN, 4)),
  (">=",	(InfixN, 4)),
  (">",		(InfixN, 4)),

  ("C:",        (InfixR, 5)),
  ("++",        (InfixR, 5)),
  ("\\",        (InfixN, 5)),
  ("!!",        (InfixL, 9)),
  (".",         (InfixR, 9)),
  ("^",         (InfixR, 8)),
  ("elem",      (InfixN, 4)),
  ("notElem",   (InfixN, 4)),

  ("||",	(InfixR, 2)),
  ("&&",	(InfixR, 3))]


main = do
    cs <- getContents
    let tokens = laMain cs
    let parser_res = parser_test tokens
    putStr (showx parser_res)

showx (PFail t) 
 = "\n\nFailed on token: " ++ show t ++  "\n\n"

showx (POk env toks result)
 = "\n\nSucceeded, with:\n   Size env = " ++ show (length env) ++
   "\n   Next token = " ++ show (head toks) ++
   "\n\n   Result = " ++ show result ++ "\n\n"

-- ==========================================================--
--
layn :: [[Char]] -> [Char]

layn x =   f 1 x
           where
           f :: Int -> [[Char]] -> [Char]
           f n [] = []
           f n (a:x) = rjustify 4 (show n) ++") "++a++"\n"++f (n+1) x



-- ==========================================================--
--
rjustify :: Int -> [Char] -> [Char]
rjustify n s = spaces (n - length s)++s
               where
                  spaces :: Int -> [Char]
                  spaces m = copy m ' '

copy :: Int -> a -> [a]

copy n x = take (max 0 n) xs where xs = x:xs

