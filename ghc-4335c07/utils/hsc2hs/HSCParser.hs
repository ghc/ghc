module HSCParser where
import Control.Applicative      hiding ( many )
import Control.Monad            ( MonadPlus(..), liftM, liftM2, ap )
import Data.Char                ( isAlpha, isAlphaNum, isSpace, isDigit )

------------------------------------------------------------------------
-- A deterministic parser which remembers the text which has been parsed.

newtype Parser a = Parser (SourcePos -> String -> ParseResult a)

runParser :: Parser a -> String -> String -> ParseResult a
runParser (Parser p) file_name = p (SourcePos file_name 1 1)

data ParseResult a = Success !SourcePos String String a
                   | Failure !SourcePos String

data SourcePos = SourcePos String !Int !Int

updatePos :: SourcePos -> Char -> SourcePos
updatePos (SourcePos name line col) ch = case ch of
    '\n' -> SourcePos name (line + 1) 1
    _    -> SourcePos name line (col + 1)

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure a = Parser $ \pos s -> Success pos [] s a
    (<*>) = ap

instance Monad Parser where
    return = pure
    Parser m >>= k =
        Parser $ \pos s -> case m pos s of
            Success pos' out1 s' a -> case k a of
                Parser k' -> case k' pos' s' of
                    Success pos'' out2 imp'' b ->
                        Success pos'' (out1++out2) imp'' b
                    Failure pos'' msg -> Failure pos'' msg
            Failure pos' msg -> Failure pos' msg
    fail msg = Parser $ \pos _ -> Failure pos msg

instance Alternative Parser where
    empty = mzero
    (<|>) = mplus

instance MonadPlus Parser where
    mzero                     = fail "mzero"
    Parser m `mplus` Parser n =
        Parser $ \pos s -> case m pos s of
            success@(Success _ _ _ _) -> success
            Failure _ _               -> n pos s

getPos :: Parser SourcePos
getPos = Parser $ \pos s -> Success pos [] s pos

setPos :: SourcePos -> Parser ()
setPos pos = Parser $ \_ s -> Success pos [] s ()

message :: Parser a -> String -> Parser a
Parser m `message` msg =
    Parser $ \pos s -> case m pos s of
        success@(Success _ _ _ _) -> success
        Failure pos' _            -> Failure pos' msg

catchOutput_ :: Parser a -> Parser String
catchOutput_ (Parser m) =
    Parser $ \pos s -> case m pos s of
        Success pos' out s' _ -> Success pos' [] s' out
        Failure pos' msg      -> Failure pos' msg

fakeOutput :: Parser a -> String -> Parser a
Parser m `fakeOutput` out =
    Parser $ \pos s -> case m pos s of
        Success pos' _ s' a -> Success pos' out s' a
        Failure pos' msg    -> Failure pos' msg

lookAhead :: Parser String
lookAhead = Parser $ \pos s -> Success pos [] s s

satisfy :: (Char -> Bool) -> Parser Char
satisfy p =
    Parser $ \pos s -> case s of
        c:cs | p c -> Success (updatePos pos c) [c] cs c
        _          -> Failure pos "Bad character"

satisfy_ :: (Char -> Bool) -> Parser ()
satisfy_ p = satisfy p >> return ()

char_ :: Char -> Parser ()
char_ c = do
    satisfy_ (== c) `message` (show c++" expected")

anyChar_ :: Parser ()
anyChar_ = do
    satisfy_ (const True) `message` "Unexpected end of file"

any2Chars_ :: Parser ()
any2Chars_ = anyChar_ >> anyChar_

many :: Parser a -> Parser [a]
many p = many1 p `mplus` return []

many1 :: Parser a -> Parser [a]
many1 p = liftM2 (:) p (many p)

many_ :: Parser a -> Parser ()
many_ p = many1_ p `mplus` return ()

many1_ :: Parser a -> Parser ()
many1_ p = p >> many_ p

manySatisfy, manySatisfy1 :: (Char -> Bool) -> Parser String
manySatisfy  = many  . satisfy
manySatisfy1 = many1 . satisfy

manySatisfy_, manySatisfy1_ :: (Char -> Bool) -> Parser ()
manySatisfy_  = many_  . satisfy
manySatisfy1_ = many1_ . satisfy

------------------------------------------------------------------------
-- Parser of hsc syntax.

data Token
    = Text    SourcePos String
    | Special SourcePos String String

tokenIsSpecial :: Token -> Bool
tokenIsSpecial (Text    {}) = False
tokenIsSpecial (Special {}) = True

parser :: Parser [Token]
parser = do
    pos <- getPos
    t <- catchOutput_ text
    s <- lookAhead
    rest <- case s of
        []  -> return []
        _:_ -> liftM2 (:) (special `fakeOutput` []) parser
    return (if null t then rest else Text pos t : rest)

text :: Parser ()
text = do
    s <- lookAhead
    case s of
        []        -> return ()
        c:_ | isAlpha c || c == '_' -> do
            anyChar_
            manySatisfy_ (\c' -> isAlphaNum c' || c' == '_' || c' == '\'')
            text
        c:_ | isHsSymbol c -> do
            symb <- catchOutput_ (manySatisfy_ isHsSymbol)
            case symb of
                "#" -> return ()
                '-':'-':symb' | all (== '-') symb' -> do
                    return () `fakeOutput` symb
                    manySatisfy_ (/= '\n')
                    text
                _ -> do
                    return () `fakeOutput` unescapeHashes symb
                    text
        '\"':_    -> do anyChar_; hsString '\"'; text
        '\'':_    -> do anyChar_; hsString '\''; text
        '{':'-':_ -> do any2Chars_; linePragma `mplus`
                                    columnPragma `mplus`
                                    hsComment; text
        _:_       -> do anyChar_; text

hsString :: Char -> Parser ()
hsString quote = do
    s <- lookAhead
    case s of
        []               -> return ()
        c:_ | c == quote -> anyChar_
        '\\':c:_
            | isSpace c  -> do
                anyChar_
                manySatisfy_ isSpace
                char_ '\\' `mplus` return ()
                hsString quote
            | otherwise  -> do any2Chars_; hsString quote
        _:_              -> do anyChar_; hsString quote

hsComment :: Parser ()
hsComment = do
    s <- lookAhead
    case s of
        []        -> return ()
        '-':'}':_ -> any2Chars_
        '{':'-':_ -> do any2Chars_; hsComment; hsComment
        _:_       -> do anyChar_; hsComment

linePragma :: Parser ()
linePragma = do
    char_ '#'
    manySatisfy_ isSpace
    satisfy_ (\c -> c == 'L' || c == 'l')
    satisfy_ (\c -> c == 'I' || c == 'i')
    satisfy_ (\c -> c == 'N' || c == 'n')
    satisfy_ (\c -> c == 'E' || c == 'e')
    manySatisfy1_ isSpace
    line <- liftM read $ manySatisfy1 isDigit
    manySatisfy1_ isSpace
    char_ '\"'
    name <- manySatisfy (/= '\"')
    char_ '\"'
    manySatisfy_ isSpace
    char_ '#'
    char_ '-'
    char_ '}'
    setPos (SourcePos name (line - 1) 1)

columnPragma :: Parser ()
columnPragma = do
    char_ '#'
    manySatisfy_ isSpace
    satisfy_ (\c -> c == 'C' || c == 'c')
    satisfy_ (\c -> c == 'O' || c == 'o')
    satisfy_ (\c -> c == 'L' || c == 'l')
    satisfy_ (\c -> c == 'U' || c == 'u')
    satisfy_ (\c -> c == 'M' || c == 'm')
    satisfy_ (\c -> c == 'N' || c == 'n')
    manySatisfy1_ isSpace
    column <- liftM read $ manySatisfy1 isDigit
    manySatisfy_ isSpace
    char_ '#'
    char_ '-'
    char_ '}'
    SourcePos name line _ <- getPos
    setPos (SourcePos name line column)

isHsSymbol :: Char -> Bool
isHsSymbol '!' = True; isHsSymbol '#' = True; isHsSymbol '$'  = True
isHsSymbol '%' = True; isHsSymbol '&' = True; isHsSymbol '*'  = True
isHsSymbol '+' = True; isHsSymbol '.' = True; isHsSymbol '/'  = True
isHsSymbol '<' = True; isHsSymbol '=' = True; isHsSymbol '>'  = True
isHsSymbol '?' = True; isHsSymbol '@' = True; isHsSymbol '\\' = True
isHsSymbol '^' = True; isHsSymbol '|' = True; isHsSymbol '-'  = True
isHsSymbol '~' = True
isHsSymbol _   = False

unescapeHashes :: String -> String
unescapeHashes []          = []
unescapeHashes ('#':'#':s) = '#' : unescapeHashes s
unescapeHashes (c:s)       = c   : unescapeHashes s

lookAheadC :: Parser String
lookAheadC = liftM joinLines lookAhead
    where
    joinLines []            = []
    joinLines ('\\':'\n':s) = joinLines s
    joinLines (c:s)         = c : joinLines s

satisfyC :: (Char -> Bool) -> Parser Char
satisfyC p = do
    s <- lookAhead
    case s of
        '\\':'\n':_ -> do any2Chars_ `fakeOutput` []; satisfyC p
        _           -> satisfy p

satisfyC_ :: (Char -> Bool) -> Parser ()
satisfyC_ p = satisfyC p >> return ()

charC_ :: Char -> Parser ()
charC_ c = satisfyC_ (== c) `message` (show c++" expected")

anyCharC_ :: Parser ()
anyCharC_ = satisfyC_ (const True) `message` "Unexpected end of file"

any2CharsC_ :: Parser ()
any2CharsC_ = anyCharC_ >> anyCharC_

manySatisfyC :: (Char -> Bool) -> Parser String
manySatisfyC = many . satisfyC

manySatisfyC_ :: (Char -> Bool) -> Parser ()
manySatisfyC_ = many_ . satisfyC

special :: Parser Token
special = do
    manySatisfyC_ (\c -> isSpace c && c /= '\n')
    s <- lookAheadC
    case s of
        '{':_ -> do
            anyCharC_
            manySatisfyC_ isSpace
            sp <- keyArg (== '\n')
            charC_ '}'
            return sp
        _ -> keyArg (const False)

keyArg :: (Char -> Bool) -> Parser Token
keyArg eol = do
    pos <- getPos
    key <- keyword `message` "hsc keyword or '{' expected"
    manySatisfyC_ (\c' -> isSpace c' && c' /= '\n' || eol c')
    arg <- catchOutput_ (argument eol)
    return (Special pos key arg)

keyword :: Parser String
keyword = do
    c  <- satisfyC (\c' -> isAlpha c' || c' == '_')
    cs <- manySatisfyC (\c' -> isAlphaNum c' || c' == '_')
    return (c:cs)

argument :: (Char -> Bool) -> Parser ()
argument eol = do
    s <- lookAheadC
    case s of
        []          -> return ()
        c:_ | eol c -> do anyCharC_;               argument eol
        '\n':_      -> return ()
        '\"':_      -> do anyCharC_; cString '\"'; argument eol
        '\'':_      -> do anyCharC_; cString '\''; argument eol
        '(':_       -> do anyCharC_; nested ')';   argument eol
        ')':_       -> return ()
        '/':'*':_   -> do any2CharsC_; cComment;   argument eol
        '/':'/':_   -> do
            any2CharsC_; manySatisfyC_ (/= '\n');  argument eol
        '[':_       -> do anyCharC_; nested ']';   argument eol
        ']':_       -> return ()
        '{':_       -> do anyCharC_; nested '}';   argument eol
        '}':_       -> return ()
        _:_         -> do anyCharC_;               argument eol

nested :: Char -> Parser ()
nested c = do argument (== '\n'); charC_ c

cComment :: Parser ()
cComment = do
    s <- lookAheadC
    case s of
        []        -> return ()
        '*':'/':_ -> do any2CharsC_
        _:_       -> do anyCharC_; cComment

cString :: Char -> Parser ()
cString quote = do
    s <- lookAheadC
    case s of
        []               -> return ()
        c:_ | c == quote -> anyCharC_
        '\\':_:_         -> do any2CharsC_; cString quote
        _:_              -> do anyCharC_; cString quote

