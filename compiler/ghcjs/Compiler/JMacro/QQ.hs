{-# LANGUAGE FlexibleInstances,
             UndecidableInstances,
             TypeFamilies,
             RankNTypes,
             GADTs,
             OverloadedStrings,
             ScopedTypeVariables
  #-}

-----------------------------------------------------------------------------
{- |
Module      :  Language.Javascript.JMacro
Copyright   :  (c) Gershom Bazerman, 2009
License     :  BSD 3 Clause
Maintainer  :  gershomb@gmail.com
Stability   :  experimental

Simple EDSL for lightweight (untyped) programmatic generation of Javascript.
-}
-----------------------------------------------------------------------------

module Compiler.JMacro.QQ (parseJM, parseJME) where

import Prelude hiding ((<*), tail, init, head, last, minimum, maximum, foldr1, foldl1, (!!), read)
import Control.Arrow (first)
import Control.Monad.State.Strict
import Data.Char (digitToInt, toLower, isAlpha)
import Data.List (isPrefixOf, sort)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Map as M
import qualified Data.Text as T

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (javaStyle)

import Compiler.JMacro.Base

import Numeric (readHex)

{--------------------------------------------------------------------
  Parsing
--------------------------------------------------------------------}

type JMParser a =  CharParser () a

lexer :: P.TokenParser ()
symbol :: String -> JMParser String
parens, braces :: JMParser a -> JMParser a
dot, colon, semi, identifier, identifierWithBang :: JMParser String
whiteSpace :: JMParser ()
reserved, reservedOp :: String -> JMParser ()
commaSep, commaSep1 :: JMParser a -> JMParser [a]

lexer = P.makeTokenParser jsLang

jsLang :: P.LanguageDef ()
jsLang = javaStyle {
           P.reservedNames = ["NaN", "Infinity", "var","return","if","else","while","for","in","break","continue","function","switch","case","default","fun","try","catch","finally","foreign","do","new","typeof","delete","instanceof","yield"],
           P.reservedOpNames = reservedOpNames,
           P.identLetter = alphaNum <|> oneOf "_$",
           P.identStart  = letter <|> oneOf "_$",
           P.opStart = oneOf "|+-/*%<>&^.?=!~:@",
           P.opLetter = oneOf "|+-/*%<>&^.?=!~:@",
           P.commentLine = "//",
           P.commentStart = "/*",
           P.commentEnd = "*/",
           P.caseSensitive = True}

identifierWithBang = P.identifier $ P.makeTokenParser $ jsLang {P.identStart = letter <|> oneOf "_$!"}

whiteSpace= P.whiteSpace lexer
symbol    = P.symbol lexer
parens    = P.parens lexer
braces    = P.braces lexer
-- brackets  = P.brackets lexer
dot       = P.dot lexer
colon     = P.colon lexer
semi      = P.semi lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOpNames :: [String]
reservedOpNames = ["+=","-=","*=","/=","%=","<<=", ">>=", ">>>=", "&=", "^=", "|=", "--","*","/","+","-",".","%","?","=","==","!=","<",">","&&","||","&", "^", "|", "++","===","!==", ">=","<=","!", "~", "<<", ">>", ">>>", "new", "typeof", "void", "delete", "instanceof", "in", "yield", "?", ":"]
reservedOp name
  | name `notElem` reservedOpNames = error ("reservedOp: not a reserved operator: " ++ name)
  | all isAlpha name = lexeme $ try $
                            string name >>
                            (notFollowedBy letter <?> ("end of " ++ show name))
  | otherwise        =
     let t  = [string (drop (length name) n) | n <- reservedOpNames, name `isPrefixOf` n, n /= name]
     in  lexeme $ try (string name >> notFollowedBy (choice t))

commaSep1 = P.commaSep1 lexer
commaSep  = P.commaSep  lexer

lexeme :: JMParser a -> JMParser a
lexeme    = P.lexeme lexer

(<*) :: Monad m => m b -> m a -> m b
x <* y = do
  xr <- x
  _ <- y
  return xr

parseJM :: String -> Either ParseError JStat
parseJM s = BlockStat <$> runParser jmacroParser () "" s
    where jmacroParser = do
            ans <- statblock
            eof
            return ans

parseJME :: String -> Either ParseError JExpr
parseJME s = runParser jmacroParserE () "" s
    where jmacroParserE = do
            ans <- whiteSpace >> expr
            eof
            return ans

-- function !foo or function foo or var !x or var x, with optional type
varidentdecl :: JMParser Ident
varidentdecl = do
  i <- identifierWithBang
  when ("jmId_" `isPrefixOf` i || "!jmId_" `isPrefixOf` i) $ fail "Illegal use of reserved jmId_ prefix in variable name."
  when (i=="this" || i=="!this") $ fail "Illegal attempt to name variable 'this'."
  return (TxtI $ T.pack i)

-- any other identifier decl
identdecl :: JMParser Ident
identdecl = do
  i <- identifier
  when ("jmId_" `isPrefixOf` i) $ fail "Illegal use of reserved jmId_ prefix in variable name."
  when (i=="this") $ fail "Illegal attempt to name variable 'this'."
  return (TxtI (T.pack i))

cleanIdent :: Ident -> Ident
cleanIdent (TxtI x) | "!" `T.isPrefixOf` x = TxtI (T.tail x)
cleanIdent x = x
{-
expr2ident :: JExpr -> Ident
expr2ident (ValExpr (JVar i)) = i
expr2ident e                  = error ("expr2ident: expected (ValExpr (JVar _)), got: " ++ show e)
-}
-- Handle varident decls for type annotations?
-- Patterns
data PatternTree = PTAs Ident PatternTree
                 | PTCons PatternTree PatternTree
                 | PTList [PatternTree]
                 | PTObj [(String, PatternTree)]
                 | PTVar Ident
                   deriving Show
patternTree :: JMParser PatternTree
patternTree = PTVar <$> varidentdecl

--either we have a function from any ident to the constituent parts
--OR the top level is named, and hence we have the top ident, plus decls for the constituent parts
patternBinding :: JMParser (Either (Ident -> [JStat]) (Ident, [JStat]))
patternBinding = do
  ptree <- patternTree
  let go path (PTAs asIdent pt) = [DeclStat asIdent, AssignStat (ValExpr (JVar (cleanIdent asIdent))) path] ++ go path pt
      go path (PTVar i)
          | i == (TxtI "_") = []
          | otherwise = [DeclStat i, AssignStat (ValExpr (JVar (cleanIdent i))) path]
      go path (PTList pts) = concatMap (uncurry go) $ zip (map addIntToPath [0..]) pts
           where addIntToPath i = IdxExpr path (ValExpr $ JInt i)
      go path (PTObj xs)   = concatMap (uncurry go) $ map (first fixPath) xs
           where fixPath lbl = IdxExpr path (ValExpr $ JStr (T.pack lbl))
      go path (PTCons x xs) = concat [go (IdxExpr path (ValExpr $ JInt 0)) x,
                                      go (ApplExpr (SelExpr path (TxtI "slice")) [ValExpr $ JInt 1]) xs]
  case ptree of
    PTVar i -> return $ Right (i,[])
    PTAs  i pt -> return $ Right (i, go (ValExpr $ JVar i) pt)
    _ -> return $ Left $ \i -> go (ValExpr $ JVar i) ptree

patternBlocks :: JMParser ([Ident], [JStat])
patternBlocks = fmap concat . unzip . zipWith (\i efr -> either (\f -> (i, f i)) id efr) (map (TxtI . T.pack . ("jmId_match_" ++) . show) [(1::Int)..]) <$> many patternBinding

destructuringDecl :: JMParser [JStat]
destructuringDecl = do
    (i,patDecls) <- either (\f -> (matchVar, f matchVar)) id <$> patternBinding
    optAssignStat <- optionMaybe $ do
       reservedOp "="
       e <- expr
       return $  AssignStat (ValExpr (JVar (cleanIdent i))) e : patDecls
    return $ DeclStat i : fromMaybe [] optAssignStat
  where matchVar = TxtI "jmId_match_var"

statblock :: JMParser [JStat]
statblock = concat <$> (sepEndBy1 (whiteSpace >> statement) (semi <|> return ""))

statblock0 :: JMParser [JStat]
statblock0 = try statblock <|> (whiteSpace >> return [])

l2s :: [JStat] -> JStat
l2s xs = BlockStat xs

statementOrEmpty :: JMParser [JStat]
statementOrEmpty = try emptyStat <|> statement
    where emptyStat = braces (whiteSpace >> return [])

-- return either an expression or a statement
statement :: JMParser [JStat]
statement = declStat
            <|> funDecl
            <|> functionDecl
            <|> returnStat
            <|> labelStat
            <|> ifStat
            <|> whileStat
            <|> switchStat
            <|> forStat
            <|> doWhileStat
            <|> braces statblock
            <|> assignOpStat
            <|> tryStat
            <|> applStat
            <|> breakStat
            <|> continueStat
          <?> "statement"
    where
      declStat = do
        reserved "var"
        res <- concat <$> commaSep1 destructuringDecl
        _ <- semi
        return res

      functionDecl = do
        reserved "function"

        i <- varidentdecl
        (as,patDecls) <- fmap (\x -> (x,[])) (try $ parens (commaSep identdecl)) <|> patternBlocks
        b' <- try (ReturnStat <$> braces expr) <|> (l2s <$> statement)
        let b = BlockStat patDecls `mappend` b'
        return $ [DeclStat i,
                  AssignStat (ValExpr $ JVar (cleanIdent i)) (ValExpr $ JFunc as b)]

      funDecl = do
        reserved "fun"
        n <- identdecl
        (as, patDecls) <- patternBlocks
        b' <- try (ReturnStat <$> braces expr) <|> (l2s <$> statement) <|> (symbol "->" >> ReturnStat <$> expr)
        let b = BlockStat patDecls `mappend` b'
        return $ [DeclStat (addBang n),
                  AssignStat (ValExpr $ JVar n) (ValExpr $ JFunc as b)]
            where addBang (TxtI x) = TxtI (T.pack "!!" `mappend` x)

      returnStat =
        reserved "return" >> (:[]) . ReturnStat <$> option (ValExpr $ JVar $ TxtI "undefined") expr

      ifStat = do
        reserved "if"
        p <- parens expr
        b <- l2s <$> statementOrEmpty
        isElse <- (lookAhead (reserved "else") >> return True)
                  <|> return False
        if isElse
          then do
            reserved "else"
            return . IfStat p b . l2s <$> statementOrEmpty
          else return $ [IfStat p b nullStat]

      whileStat =
          reserved "while" >> liftM2 (\e b -> [WhileStat False e (l2s b)])
                              (parens expr) statementOrEmpty

      doWhileStat = reserved "do" >> liftM2 (\b e -> [WhileStat True e (l2s b)])
                    statementOrEmpty (reserved "while" *> parens expr)

      switchStat = do
        reserved "switch"
        e <- parens $ expr
        (l,d) <- braces (liftM2 (,) (many caseStat) (option [] dfltStat))
        return $ [SwitchStat e l (l2s d)]

      caseStat =
        reserved "case" >> liftM2 (,) expr (char ':' >> l2s <$> statblock0)

      tryStat = do
        reserved "try"
        s <- statement
        isCatch <- (lookAhead (reserved "catch") >> return True)
                  <|> return False
        (i,s1) <- if isCatch
                  then do
                    reserved "catch"
                    liftM2 (,) (parens identdecl) statement
                  else return $ (TxtI "", [])
        isFinally <- (lookAhead (reserved "finally") >> return True)
                  <|> return False
        s2 <- if isFinally
                then reserved "finally" >> statement
                else return $ []
        return [TryStat (BlockStat s) i (BlockStat s1) (BlockStat s2)]


      dfltStat =
        reserved "default" >> char ':' >> whiteSpace >> statblock

      forStat =
        reserved "for" >> ((reserved "each" >> inBlock True)
                           <|> try (inBlock False)
                           <|> simpleForStat)

      inBlock isEach = do
        char '(' >> whiteSpace
        dec <- isJust <$> optionMaybe (reserved "var")
        i <- identdecl
        reserved "in"
        e <- expr
        char ')' >> whiteSpace
        s <- l2s <$> statement
        return $ (if dec then (DeclStat i:) else id) [ForInStat isEach i e s]

      simpleForStat = do
        (before,after,p) <- parens threeStat
        jFor' before after p <$> statement
          where threeStat =
                    liftM3 (,,) (option [] statement <* optional semi)
                                (optionMaybe expr <* semi)
                                (option [] statement)
                jFor' :: [JStat] -> Maybe JExpr -> [JStat] -> [JStat] -> [JStat]
                jFor' before p after bs = before ++ [WhileStat False (fromMaybe (jsv "true") p) b']
                    where b' = BlockStat $ bs ++ after

      assignOpStat = do
          let rop x x' = reservedOp x >> return x'
          (e1, op) <- try $ liftM2 (,) dotExpr (
                                                   rop "=" Nothing
                                               <|> rop "+=" (Just AddOp)
                                               <|> rop "-=" (Just SubOp)
                                               <|> rop "*=" (Just MulOp)
                                               <|> rop "/=" (Just DivOp)
                                               <|> rop "%=" (Just ModOp)
                                               <|> rop "<<=" (Just LeftShiftOp)
                                               <|> rop ">>=" (Just RightShiftOp)
                                               <|> rop ">>>=" (Just ZRightShiftOp)
                                               <|> rop "&=" (Just BAndOp)
                                               <|> rop "^=" (Just BXorOp)
                                               <|> rop "|=" (Just BOrOp)
                                              )
          let gofail  = fail "Invalid assignment."
              badList = ["this", "true", "false", "undefined", "null"]
          case e1 of
            ValExpr (JVar (TxtI s)) -> if s `elem` badList then gofail else return ()
            ApplExpr {}             -> gofail
            ValExpr {}              -> gofail
            _                       -> return ()
          e2 <- expr
          return [AssignStat e1 $ maybe e2 (\o -> InfixExpr o e1 e2) op]

      applStat = expr2stat' =<< expr

-- fixme: don't handle ifstats
      expr2stat' e = case expr2stat e of
                       BlockStat [] -> pzero
                       x -> return [x]
{-
      expr2stat' :: JExpr -> JStat
      expr2stat' (ApplExpr x y) = return $ (ApplStat x y)
      expr2stat' (IfExpr x y z) = liftM2 (IfStat x) (expr2stat' y) (expr2stat' z)
      expr2stat' (PostExpr s x) = return $ PostStat s x
      expr2stat' (AntiExpr x)   = return $ AntiStat x
      expr2stat' _ = fail "Value expression used as statement"
-}

      breakStat = do
        reserved "break"
        l <- optionMaybe myIdent
        return [BreakStat (T.pack <$> l)]

      continueStat = do
        reserved "continue"
        l <- optionMaybe myIdent
        return [ContinueStat (T.pack <$> l)]

      labelStat = do
        lbl <- try $ do
                    l <- myIdent <* char ':'
                    guard (l /= "default")
                    return l
        s <- l2s <$> statblock0
        return [LabelStat (T.pack lbl) s]

expr :: JMParser JExpr
expr = exprWithIf
  where
    exprWithIf = do
         e <- rawExpr
         addIf e <|> return e
    addIf e = do
          reservedOp "?"
          t <- exprWithIf
          _ <- colon
          el <- exprWithIf
          let ans = (IfExpr e t el)
          addIf ans <|> return ans
    rawExpr = buildExpressionParser table dotExpr <?> "expression"
    table = [[pop "++" PreInc, pop "--" PreDec, poop "++" PostInc, poop "--" PostDec],  -- fixme, yield
             [pop "~" BNotOp, pop "!" NotOp, negop, pop "+" PlusOp, pop "typeof" TypeofOp, pop "void" VoidOp, pop "delete" DeleteOp],
             [iop "*" MulOp, iop "/" DivOp, iop "%" ModOp],
             [iop "+" AddOp, iop "-" SubOp],
             [iop "<<" LeftShiftOp, iop ">>>" ZRightShiftOp, iop ">>" RightShiftOp],
             [iop "<=" LeOp, iop "<" LtOp, iop ">=" GeOp, iop ">" GtOp, iop "in" InOp, iop "instanceof" InstanceofOp],
             [iop "===" StrictEqOp, iop "!==" StrictNeqOp, iop "==" EqOp, iop "!=" NeqOp],
             [iop "&" BAndOp],
             [iop "^" BXorOp],
             [iop "|" BOrOp],
             [iop "&&" LAndOp],
             [iop "||" LOrOp]
            ]
    pop  s s' = Prefix (reservedOp s >> return (UOpExpr s'))
    poop s s' = Postfix (reservedOp s >> return (UOpExpr s'))
    iop  s s' = Infix (reservedOp s >> return (InfixExpr s')) AssocLeft
    negop   = Prefix (reservedOp "-" >> return negexp)
    negexp (ValExpr (JDouble n)) = ValExpr (JDouble (-n))
    negexp (ValExpr (JInt    n)) = ValExpr (JInt    (-n))
    negexp x                     = UOpExpr NegOp x

dotExpr :: JMParser JExpr
dotExpr = do
  e <- many1 (lexeme dotExprOne) <?> "simple expression"
  case e of
    [e'] -> return e'
    (e':es) -> return $ ApplExpr e' es
    _ -> error "dotExpr: exprApp"

dotExprOne :: JMParser JExpr
dotExprOne = addNxt =<< valExpr <|> parens' expr <|> newExpr
  where
    addNxt e = do
            nxt <- (Just <$> lookAhead anyChar <|> return Nothing)
            case nxt of
              Just '.' -> addNxt =<< (dot >> (SelExpr e <$> (ident' <|> numIdent)))
              Just '[' -> addNxt =<< (IdxExpr e <$> brackets' expr)
              Just '(' -> addNxt =<< (ApplExpr e <$> parens' (commaSep expr))
              _   -> return e

    numIdent = TxtI . T.pack <$> many1 digit

    newExpr = UOpExpr NewOp <$> (reservedOp "new" >> dotExpr)

    valExpr = ValExpr <$> (num <|> str <|> try regex <|> list <|> hash <|> func <|> var) <?> "value"
        where num   = lexeme $ either JInt JDouble <$> try natFloat
              str   = lexeme $ JStr . T.pack <$> (myStringLiteral '"' <|> myStringLiteral '\'')
              regex = lexeme $ JRegEx . T.pack <$> regexLiteral
              --do
                -- s <- regexLiteral
                -- myStringLiteralNoBr '/'
                -- fixme: syntax check for regexp removed because it depends on regex-posix
                -- pure (JRegEx $ T.pack s)
              list  = lexeme $ JList  <$> brackets' (commaSep expr)
              hash  = lexeme $ JHash  . M.fromList <$> braces' (commaSep propPair)
              var   = JVar <$> ident'
              func  = lexeme $ do
                (symbol "\\" >> return ()) <|> reserved "function"
                (as,patDecls) <- fmap (\x -> (x,[])) (try $ parens (commaSep identdecl)) <|> patternBlocks
                b' <- (braces' statOrEblock <|> (symbol "->" >> (ReturnStat <$> expr)))
                return $ JFunc as (BlockStat patDecls `mappend` b')
              statOrEblock  = try (ReturnStat <$> expr `folBy` '}') <|> (l2s <$> statblock)
              propPair = liftM2 (,) (T.pack <$> myIdent) (colon >> expr)

-- notFolBy a b = a <* notFollowedBy (char b)
folBy :: JMParser a -> Char -> JMParser a
folBy a b = a <* (lookAhead (char b) >>= const (return ()))

-- Parsers without Lexeme
braces', brackets', parens' {- , oxfordBraces -} :: JMParser a -> JMParser a
brackets' = around' '[' ']'
braces' = around' '{' '}'
parens' = around' '(' ')'

around' :: Char -> Char -> JMParser a -> JMParser a
around' a b x = lexeme (char a) >> (lexeme x <* char b)

myIdent :: JMParser String
myIdent = lexeme $ many1 (alphaNum <|> oneOf "_-!@#$%^&*()") <|> myStringLiteral '\''

ident' :: JMParser Ident
ident' = lexeme $ do
    i <- identifier'
    when ("jmId_" `isPrefixOf` i) $ fail "Illegal use of reserved jmId_ prefix in variable name."
    return (TxtI $ T.pack i)
  where
    identifier' =
        try $
        do{ name <- ident''
          ; if isReservedName name
             then unexpected ("reserved word " ++ show name)
             else return name
          }
    ident''
        = do{ c <- P.identStart jsLang
            ; cs <- many (P.identLetter jsLang)
            ; return (c:cs)
            }
        <?> "identifier"
    isReservedName name
        = isReserved theReservedNames caseName
        where
          caseName      | P.caseSensitive jsLang  = name
                        | otherwise               = map toLower name
    isReserved names name
        = scan names
        where
          scan []       = False
          scan (r:rs)   = case (compare r name) of
                            LT  -> scan rs
                            EQ  -> True
                            GT  -> False
    theReservedNames
        | P.caseSensitive jsLang  = sortedNames
        | otherwise               = map (map toLower) sortedNames
        where
          sortedNames   = sort (P.reservedNames jsLang)


natFloat :: JMParser (Either Integer SaneDouble)
natFloat = (char '0' >> zeroNumFloat) <|> try nan <|> try infinity
           <|> decimalFloat <?> "number"
 where
    nan             = reserved "NaN" >> return (Right (0/0))
    infinity        = reserved "Infinity" >> return (Right (1/0))

    zeroNumFloat    =  (Left <$> (hexadecimal <|> octal))
                       <|> decimalFloat
                       <|> fractFloat 0
                       <|> return (Left 0)

    decimalFloat    = do n <- decimal
                         option (Left n)(fractFloat n)
    fractFloat n    = Right <$> fractExponent n
    fractExponent n = (do fract <- fraction
                          expo  <- option 1.0 exponent'
                          return ((fromInteger n + fract)*expo)
                      )
                      <|> ((fromInteger n *) <$> exponent')
    fraction        = char '.' >> (foldr op 0.0 <$> many1 digit <?> "fraction")
                    where
                      op d f    = (f + fromIntegral (digitToInt d))/10.0
    exponent'       = do _ <- oneOf "eE"
                         power <$> decimal
                    where
                       power e  | e < 0      = 1.0/power(-e)
                                | otherwise  = fromInteger (10^e)

    decimal         = number 10 digit
    hexadecimal     = oneOf "xX" >> number 16 hexDigit
    octal           = oneOf "oO" >> number 8 octDigit

    number base baseDig = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 <$> many1 baseDig

myStringLiteral :: Char -> JMParser String
myStringLiteral t = do
    _ <- char t
    x <- concat <$> many myChar
    _ <- char t
    decodeJson x
 where myChar = do
         c <- noneOf [t]
         case c of
           '\\' -> do
                  c2 <- anyChar
                  return [c,c2]
           _ -> return [c]

-- Taken from json package by Sigbjorn Finne.
decodeJson :: String -> JMParser String
decodeJson x = parseIt [] x
 where
  parseIt rs cs =
    case cs of
      '\\' : c : ds -> esc rs c ds
      c    : ds
       | c >= '\x20' && c <= '\xff' -> parseIt (c:rs) ds
       | c < '\x20'     -> fail $ "Illegal unescaped character in string: " ++ x
       | i <= 0x10ffff  -> parseIt (c:rs) ds
       | otherwise -> fail $ "Illegal unescaped character in string: " ++ x
       where
        i = (fromIntegral (fromEnum c) :: Integer)
      [] -> return $ reverse rs

  esc rs c cs = case c of
   '\\' -> parseIt ('\\' : rs) cs
   '"'  -> parseIt ('"'  : rs) cs
   'n'  -> parseIt ('\n' : rs) cs
   'r'  -> parseIt ('\r' : rs) cs
   't'  -> parseIt ('\t' : rs) cs
   'f'  -> parseIt ('\f' : rs) cs
   'b'  -> parseIt ('\b' : rs) cs
   '/'  -> parseIt ('/'  : rs) cs
   'u'  -> case cs of
             d1 : d2 : d3 : d4 : cs' ->
               case readHex [d1,d2,d3,d4] of
                 [(n,"")] -> parseIt (toEnum n : rs) cs'

                 badHex -> fail $ "Unable to parse JSON String: invalid hex: " ++ show badHex
             _ -> fail $ "Unable to parse JSON String: invalid hex: " ++ cs
   _ ->  fail $ "Unable to parse JSON String: invalid escape char: " ++ [c]

--tricky bit to deal with regex literals and comments / / -- if we hit // inside, then we fail, since that isn't ending the regex but introducing a comment, and thus the initial / could not have introduced a regex.
regexLiteral :: JMParser String
regexLiteral = do
    _ <- char '/'
    x <- concat <$> many myChar
    _ <- char '/'
    b <- option False (char '/' >> return True)
    if b
       then mzero
       else return x
 where myChar = do
         c <- noneOf ['/','\n']
         case c of
           '\\' -> do
                  c2 <- anyChar
                  return [c,c2]
           _ -> return [c]
