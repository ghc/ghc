module ParsePP (
    parseCppParser,
    plusTimesExpr,
    -- testing, delete
    cppDefinition,
) where

import Data.Char

import Control.Monad (void)
import Data.Functor.Identity
import Debug.Trace
import GHC.Parser.Errors.Ppr ()
import Text.Parsec
import qualified Text.Parsec as Parsec
import Text.Parsec.Char as PS
import Text.Parsec.Combinator as PS
import qualified Text.Parsec.Expr as E
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim as PS hiding (token)
import qualified Text.Parsec.Token as P

import qualified Parser
import Types

-- =====================================================================
-- First parse to CPP tokens, using a C++-like language spec
-- https://gcc.gnu.org/onlinedocs/cpp/Tokenization.html

lexer :: P.TokenParser ()
lexer = P.makeTokenParser exprDef

exprDef :: P.LanguageDef st
exprDef =
    emptyDef
        { P.commentStart = "/*"
        , P.commentEnd = "*/"
        , P.commentLine = "//"
        , P.nestedComments = False
        , P.identStart = letter <|> char '_'
        , P.identLetter = alphaNum <|> oneOf "_'"
        , P.opStart = P.opLetter exprDef
        , P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
        , P.reservedOpNames = []
        , P.reservedNames = []
        , P.caseSensitive = True
        }

-- =====================================================================
-- ---------------------------------------------------------------------

type CppParser = Parsec String ()


-- parseDirective :: String -> Either String CppDirective
-- parseDirective = Parser.parseDirective

parseDirectiveOld :: String -> Either Parsec.ParseError CppDirective
parseDirectiveOld = parseCppParser cppDirective

parseCppParser :: CppParser a -> String -> Either Parsec.ParseError a
parseCppParser p = PS.parse p ""

-- TODO: delete this
cppDefinition :: CppParser (String, [String])
cppDefinition = do
    _ <- PS.char '#'
    _ <- whiteSpace
    eToken "define"
    name <- cppToken
    definition <- cppTokens
    return (name, definition)

cppDirective :: CppParser CppDirective
cppDirective = do
    _ <- PS.char '#'
    _ <- whiteSpace
    choice
        [ cppKw "define" >> cmdDefinition
        , try $ cppKw "include" >> cmdInclude
        , try $ cppKw "ifdef" >> cmdIfdef
        , try $ cppKw "ifndef" >> cmdIfndef
        -- , try $ cppKw "if" >> cmdIf
        , try $ cppKw "else" >> return CppElse
        , cppKw "endif" >> return CppEndif
        -- , cppKw "elif" CppElifKw
        -- , cppKw "undef" CppUndefKw
        -- , cppKw "error" CppErrorKw
        ]

cmdInclude :: CppParser CppDirective
cmdInclude = do
    _ <- string "\""
    filename <- many1 (satisfy (\c -> not (isSpace c || c == '"')))
    _ <- string "\""
    return $ CppInclude filename

cmdDefinition :: CppParser CppDirective
cmdDefinition = do
    name <- cppToken
    CppDefine name <$> cppTokens

cmdIfdef :: CppParser CppDirective
cmdIfdef = CppIfdef <$> cppToken

cmdIfndef :: CppParser CppDirective
cmdIfndef = CppIfndef <$> cppToken

-- cmdIf :: CppParser CppDirective
-- cmdIf = CppIf <$> cppTokens

cppKw :: String -> CppParser ()
cppKw kw = void $ lexeme (PS.string kw)

cppComment :: CppParser ()
cppComment = do
    _ <- PS.string "/*"
    _ <- PS.manyTill PS.anyChar (PS.try (PS.string "*/"))
    return ()

whiteSpace :: CppParser ()
whiteSpace = do
    _ <- PS.many (PS.choice [cppComment, void PS.space])
    return ()

lexeme :: CppParser a -> CppParser a
lexeme p = p <* whiteSpace

cppToken :: CppParser String
cppToken = lexeme (PS.many1 (PS.satisfy (not . isSpace)))

cppTokens :: CppParser [String]
cppTokens = PS.many cppToken

-- -------------------------------------

plusTimesExpr :: CppParser Expr
plusTimesExpr = E.buildExpressionParser eTable eTerm

eTable :: [[E.Operator String () Data.Functor.Identity.Identity Expr]]
eTable =
  []
    -- -- Via https://learn.microsoft.com/en-us/cpp/cpp/cpp-built-in-operators-precedence-and-associativity?view=msvc-170
    -- [ [E.Infix (Times <$ symbol "*") E.AssocLeft]
    -- , [E.Infix (Plus <$ symbol "+") E.AssocLeft]
    -- ,
    --     [ E.Infix (try $ BinOp CmpLtE <$ symbol "<=") E.AssocLeft
    --     , E.Infix (try $ BinOp CmpGtE <$ symbol ">=") E.AssocLeft
    --     , E.Infix (BinOp CmpLt <$ symbol "<") E.AssocLeft
    --     , E.Infix (BinOp CmpGt <$ symbol ">") E.AssocLeft
    --     ]
    -- , [E.Infix (BinOp CmpEqual <$ symbol "==") E.AssocLeft]
    -- , [E.Infix (BinOp LogicalAnd <$ symbol "&&") E.AssocLeft]
    -- , [E.Infix (BinOp LogicalOr <$ symbol "||") E.AssocLeft]
    -- ]

eTerm :: CppParser Expr
eTerm =
    eVariable -- <|> pteNum
        <|> pteParens
        <|> eInteger

pteParens :: CppParser Expr
pteParens = Parens <$> between (symbol "(") (symbol ")") plusTimesExpr

symbol :: String -> CppParser String
symbol s = lexeme $ string s

-- -- -------------------------------------

-- eExpr :: CppParser Expr
-- eExpr = choice [eParens, eBinOp, eVariable]

-- eParens :: CppParser Expr
-- eParens = P.parens lexer $ do
--     Parens <$> eExpr

-- eBinOp :: CppParser Expr
-- eBinOp = do
--     e1 <- eExpr
--     op <- eOp
--     -- _ <- cppToken
--     -- let op = Or
--     BinOp op e1 <$> eExpr

-- eOp :: CppParser Op
-- eOp = do
--     -- op <- P.operator lexer
--     op <- P.operator (trace "foo" lexer)
--     return $ trace ("op=" ++ show op) LogicalOr

-- TODO: Do we need this? the expression should be fully expanded by
-- the time we get it
eVariable :: CppParser Expr
eVariable = do
    v <- P.identifier lexer
    return $ Var v

eToken :: String -> CppParser ()
eToken = P.reserved lexer

eInteger :: CppParser Expr
eInteger = IntVal <$> integer

integer :: CppParser Int
integer = read <$> lexeme (many1 digit)

-- ---------------------------------------------------------------------

doATest :: String -> Either String CppDirective
doATest str = Parser.parseDirective str
-- doATest str = parseDirectiveOld str

t0 :: Either String CppDirective
t0 = doATest "#define FOO(m1,m2,m) ((m1) <  1 || (m1) == 1 && (m2) <  7 || (m1) == 1 && (m2) == 7 && (m) <= 0)"

-- Right (CppDefine "FOO(m1,m2,m)" ["((m1)","<","1","||","(m1)","==","1","&&","(m2)","<","7","||","(m1)","==","1","&&","(m2)","==","7","&&","(m)","<=","0)"])

t1 :: Either String CppDirective
t1 = doATest "#if (m < 1)"

t2 :: Either String CppDirective
t2 = doATest "# if ((m1) <  1 || (m1) == 1 && (m2) <  7 || (m1) == 1 && (m2) == 7 && (m) <= 0)"

-- Right (CppIf
--        (Logic LogicalOr
--         (Logic LogicalOr
--          (Comp CmpLt (Var "m1") (IntVal 1))
--          (Logic LogicalAnd
--           (Comp CmpEqual (Var "m1") (IntVal 1))
--           (Comp CmpLt (Var "m2") (IntVal 7))))
--         (Logic LogicalAnd
--           (Logic LogicalAnd
--            (Comp CmpEqual (Var "m1") (IntVal 1))
--            (Comp CmpEqual (Var "m2") (IntVal 7)))
--           (Comp CmpLtE (Var "m") (IntVal 0)))))

t3 :: Either String CppDirective
t3 = Parser.parseDirective "# if FOO == 4"
