module Parse where

import Data.Char
import GHC.Parser.Errors.Ppr ()
import qualified Text.Parsec as Parsec
import Text.Parsec.Char as PS
import Text.Parsec.Combinator as PS
import Text.Parsec.Prim as PS
import Text.Parsec.String (Parser)

-- import Debug.Trace

-- ---------------------------------------------------------------------

type CppParser = Parsec String ()

regularParse :: Parser a -> String -> Either Parsec.ParseError a
regularParse p = PS.parse p ""

-- TODO: delete this
cppDefinition :: CppParser (String, [String])
cppDefinition = do
    _ <- PS.char '#'
    _ <- whiteSpace
    _ <- lexeme (PS.string "define")
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

-- ---------------------------------------------------------------------
-- import GHC.S.Types

-- parseS :: String -> Either ParseError S
-- parseS = parse expr ""

-- lexer :: P.TokenParser ()
-- lexer =
--     P.makeTokenParser
--         ( emptyDef
--             { P.reservedNames =
--                 [ "define"
--                 , "include"
--                 , "undef"
--                 , "error"
--                 , "ifdef"
--                 , "ifndef"
--                 , "if"
--                 , "elif"
--                 , "else"
--                 , "endif"
--                 ]
--             }
--         )

-- slam :: Parser S
-- slam = P.parens lexer $ do
--     P.reserved lexer "lam"
--     ident <- P.identifier lexer
--     body <- expr
--     return (SLam (Atom ident) body)

-- slet :: Parser S
-- slet = P.parens lexer $ do
--     P.reserved lexer "let"
--     (ident, e1) <- P.parens lexer $ do
--         idnt <- P.identifier lexer
--         expr1 <- expr
--         return (idnt, expr1)
--     e2 <- expr
--     return (SLet (Atom ident) e1 e2)

-- sletrec :: Parser S
-- sletrec = P.parens lexer $ do
--     P.reserved lexer "letrec"
--     ls <- P.parens lexer $ many1 $ P.parens lexer $ do
--         idnt <- P.identifier lexer
--         expr1 <- expr
--         return (Atom idnt, expr1)
--     e2 <- expr
--     return (SLetRec ls e2)

-- scase :: Parser S
-- scase = P.parens lexer $ do
--     P.reserved lexer "case"
--     e <- expr
--     alt <- optionMaybe (P.identifier lexer)
--     alts <- P.parens lexer $ many1 $ P.parens lexer $ do
--         pat <- expr
--         ex <- expr
--         return (pat, ex)
--     case alt of
--         Just alt -> return (SCase (Atom alt) e alts)
--         Nothing -> return (SCase (Atom "_") e alts)

-- swild :: Parser S
-- swild = do
--     P.symbol lexer "_"
--     return SWild

-- sbinop :: Parser S
-- sbinop = P.parens lexer $ do
--     e1 <- expr
--     op <- P.operator lexer
--     e2 <- expr
--     return (SBinOp (Atom op) e1 e2)

-- expr :: Parser S
-- expr =
--     choice
--         [ try slam
--         , try sbinop
--         , try slet
--         , try sletrec
--         , try scase
--         , STuple <$> P.parens lexer (many expr)
--         , swild
--         , SAtom <$> (Atom <$> (P.identifier lexer))
--         , SString <$> P.stringLiteral lexer
--         , SInt <$> P.integer lexer
--         ]
