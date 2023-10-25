module ParseOld where

import Data.Char

import Data.Functor.Identity
import GHC.Parser.Errors.Ppr ()
import Text.Parsec
import qualified Text.Parsec as Parsec
import Text.Parsec.Char as PS
import Text.Parsec.Combinator as PS
import qualified Text.Parsec.Expr as E
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim as PS hiding (token)
import Text.Parsec.String

-- import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P

import Debug.Trace

-- ---------------------------------------------------------------------

type CppParser = Parsec String ()

regularParse :: Parser a -> String -> Either Parsec.ParseError a
regularParse p = PS.parse p ""

-- TODO: delete this
cppDefinition :: CppParser (String, [String])
cppDefinition = do
    _ <- PS.char '#'
    _ <- whiteSpace
    eToken "define"
    name <- cppToken
    definition <- cppTokens
    return (name, definition)

data CppDirective
    = CppInclude String
    | CppDefine String [String]
    | CppIfdef String
    | CppIfndef String
    | CppElse
    | CppEndif
    deriving (Show, Eq)

cppDirective :: CppParser CppDirective
cppDirective = do
    _ <- PS.char '#'
    _ <- whiteSpace
    choice
        [ cppKw "define" >> cmdDefinition
        , try $ cppKw "include" >> cmdInclude
        , try $ cppKw "ifdef" >> cmdIfdef
        , cppKw "ifndef" >> cmdIfndef
        , try $ cppKw "else" >> return CppElse
        , cppKw "endif" >> return CppEndif
        -- , cppKw "if" CppIfKw
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
    definition <- cppTokens
    return $ CppDefine name definition

cmdIfdef :: CppParser CppDirective
cmdIfdef = do
    name <- cppToken
    return $ CppIfdef name

cmdIfndef :: CppParser CppDirective
cmdIfndef = do
    name <- cppToken
    return $ CppIfndef name

cppKw :: String -> CppParser ()
cppKw kw = do
    _ <- lexeme (PS.string kw)
    return ()

cppComment :: CppParser ()
cppComment = do
    _ <- PS.string "/*"
    _ <- PS.manyTill PS.anyChar (PS.try (PS.string "*/"))
    return ()

whiteSpace :: CppParser ()
whiteSpace = do
    _ <- PS.many (PS.choice [cppComment, PS.space >> return ()])
    return ()

lexeme :: CppParser a -> CppParser a
lexeme p = p <* whiteSpace

cppToken :: CppParser String
cppToken = lexeme (PS.many1 (PS.satisfy (\c -> not (isSpace c))))

cppTokens :: CppParser [String]
cppTokens = PS.many cppToken

-- token :: String -> CppParser ()
-- token str = do
--     _ <- lexeme (PS.string str)
--     return ()

-- ---------------------------------------------------------------------
-- Expression language
-- NOTE: need to take care of macro expansion while parsing. Or perhaps before?

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

data Expr
    = Parens Expr
    | Var String
    | IntVal Int
    | Plus Expr Expr
    | Times Expr Expr
    | BinOp Op Expr Expr
    deriving (Show)

data Op
    = Or
    | And
    | CmpEqual
    | CmpGt
    | CmpGtE
    | CmpLt
    | CmpLtE
    deriving (Show)

-- -------------------------------------

plusTimesExpr :: CppParser Expr
plusTimesExpr = E.buildExpressionParser eTable eTerm

eTable :: [[E.Operator String () Data.Functor.Identity.Identity Expr]]
eTable =
    [ [E.Infix (Times <$ symbol "*") E.AssocLeft]
    , [E.Infix (Plus <$ symbol "+") E.AssocLeft]
    , [E.Infix (BinOp CmpLt <$ symbol "<") E.AssocLeft]
    ]

eTerm :: CppParser Expr
eTerm = eVariable -- <|> pteNum
        <|> pteParens
        <|> eInteger

pteParens :: CppParser Expr
pteParens = Parens <$> between (symbol "(") (symbol ")") plusTimesExpr

symbol :: String -> CppParser String
symbol s = lexeme $ string s

-- expr    = E.buildExpressionParser table term
--          <?> "expression"

-- term    =  parens expr
--          <|> natural
--          <?> "simple expression"

-- table   = [ [prefix "-" negate, prefix "+" id ]
--            , [postfix "++" (+1)]
--            , [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft ]
--            , [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
--            ]

-- binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
-- prefix  name fun       = Prefix (do{ reservedOp name; return fun })
-- postfix name fun       = Postfix (do{ reservedOp name; return fun })

-- -------------------------------------

eExpr :: CppParser Expr
eExpr = choice [eParens, eBinOp, eVariable]

eParens :: CppParser Expr
eParens = P.parens lexer $ do
    e <- eExpr
    return $ Parens e

eBinOp :: CppParser Expr
eBinOp = do
    e1 <- eExpr
    op <- eOp
    -- _ <- cppToken
    -- let op = Or
    e2 <- eExpr
    return $ BinOp op e1 e2

eOp :: CppParser Op
eOp = do
    -- op <- P.operator lexer
    op <- P.operator (trace "foo" lexer)
    return $ trace ("op=" ++ show op) Or

eVariable :: CppParser Expr
eVariable = do
    v <- P.identifier lexer
    return $ Var v

eToken :: String -> CppParser ()
eToken str = P.reserved lexer str

eInteger :: Parser Expr
eInteger = IntVal <$> integer

integer :: Parser Int
integer = read <$> lexeme (many1 digit)

-- ---------------------------------------------------------------------

doATest :: String -> Either Parsec.ParseError CppDirective
doATest str =
    regularParse cppDirective str

t0 :: Either Parsec.ParseError CppDirective
t0 = doATest "#define FOO(m1,m2,m) ((m1) <  1 || (m1) == 1 && (m2) <  7 || (m1) == 1 && (m2) == 7 && (m) <= 0)"

t1 :: Either Parsec.ParseError Expr
-- t1 = regularParse eExpr "(m < 1)"
t1 = regularParse plusTimesExpr "(m < 1)"
