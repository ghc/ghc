module Haddock.Backends.Hyperlinker.Parser (parse) where


import Data.Char
import Data.List
import Data.Maybe

import Haddock.Backends.Hyperlinker.Types


-- | Turn source code string into a stream of more descriptive tokens.
--
-- Result should retain original file layout (including comments, whitespace,
-- etc.), i.e. the following "law" should hold:
--
-- @concat . map 'tkValue' . 'parse' = id@
parse :: String -> [Token]
parse = tokenize . tag . chunk

-- | Split raw source string to more meaningful chunks.
--
-- This is the initial stage of tokenization process. Each chunk is either
-- a comment (including comment delimiters), a whitespace string, preprocessor
-- macro (and all its content until the end of a line) or valid Haskell lexeme.
chunk :: String -> [String]
chunk [] = []
chunk str@(c:_)
    | isSpace c =
        let (space, mcpp, rest) = spanSpaceOrCpp str
        in [space] ++ maybeToList mcpp ++ chunk rest
chunk str
    | "--" `isPrefixOf` str = chunk' $ spanToNewline str
    | "{-" `isPrefixOf` str = chunk' $ chunkComment 0 str
    | otherwise = case lex' str of
        (tok:_) -> chunk' tok
        [] -> [str]
  where
    chunk' (c, rest) = c:(chunk rest)

-- | A bit better lexer then the default, i.e. handles DataKinds quotes
lex' :: ReadS String
lex' ('\'' : '\'' : rest)              = [("''", rest)]
lex' str@('\'' : '\\' : _ : '\'' : _)  = lex str
lex' str@('\'' : _ : '\'' : _)         = lex str
lex' ('\'' : rest)                     = [("'", rest)]
lex' str                               = lex str

-- | Split input to "first line" string and the rest of it.
--
-- Ideally, this should be done simply with @'break' (== '\n')@. However,
-- Haskell also allows line-unbreaking (or whatever it is called) so things
-- are not as simple and this function deals with that.
spanToNewline :: String -> (String, String)
spanToNewline [] = ([], [])
spanToNewline ('\\':'\n':str) =
    let (str', rest) = spanToNewline str
    in ('\\':'\n':str', rest)
spanToNewline str@('\n':_) = ("", str)
spanToNewline (c:str) =
    let (str', rest) = spanToNewline str
    in (c:str', rest)

-- | Split input to whitespace string, (optional) preprocessor directive and
-- the rest of it.
--
-- Again, using something like @'span' 'isSpace'@ would be nice to chunk input
-- to whitespace. The problem is with /#/ symbol - if it is placed at the very
-- beginning of a line, it should be recognized as preprocessor macro. In any
-- other case, it is ordinary Haskell symbol and can be used to declare
-- operators. Hence, while dealing with whitespace we also check whether there
-- happens to be /#/ symbol just after a newline character - if that is the
-- case, we begin treating the whole line as preprocessor macro.
spanSpaceOrCpp :: String -> (String, Maybe String, String)
spanSpaceOrCpp ('\n':'#':str) =
    let (str', rest) = spanToNewline str
    in ("\n", Just $ '#':str', rest)
spanSpaceOrCpp (c:str')
    | isSpace c =
        let (space, mcpp, rest) = spanSpaceOrCpp str'
        in (c:space, mcpp, rest)
spanSpaceOrCpp str = ("", Nothing, str)

-- | Split input to comment content (including delimiters) and the rest.
--
-- Again, some more logic than simple 'span' is required because of Haskell
-- comment nesting policy.
chunkComment :: Int -> String -> (String, String)
chunkComment _ [] = ("", "")
chunkComment depth ('{':'-':str) =
    let (c, rest) = chunkComment (depth + 1) str
    in ("{-" ++ c, rest)
chunkComment depth ('-':'}':str)
    | depth == 1 = ("-}", str)
    | otherwise =
        let (c, rest) = chunkComment (depth - 1) str
        in ("-}" ++ c, rest)
chunkComment depth (e:str) =
    let (c, rest) = chunkComment depth str
    in (e:c, rest)

-- | Assign source location for each chunk in given stream.
tag :: [String] -> [(Span, String)]
tag =
    reverse . snd . foldl aux (Position 1 1, [])
  where
    aux (pos, cs) str =
        let pos' = foldl move pos str
        in (pos', (Span pos pos', str):cs)
    move pos '\n' = pos { posRow = posRow pos + 1, posCol = 1 }
    move pos _ = pos { posCol = posCol pos + 1 }

-- | Turn unrecognised chunk stream to more descriptive token stream.
tokenize :: [(Span, String)] -> [Token]
tokenize =
    map aux
  where
    aux (sp, str) = Token
        { tkType = classify str
        , tkValue = str
        , tkSpan = sp
        }

-- | Classify given string as appropriate Haskell token.
--
-- This method is based on Haskell 98 Report lexical structure description:
-- https://www.haskell.org/onlinereport/lexemes.html
--
-- However, this is probably far from being perfect and most probably does not
-- handle correctly all corner cases.
classify :: String -> TokenType
classify str
    | "--" `isPrefixOf` str = TkComment
    | "{-#" `isPrefixOf` str = TkPragma
    | "{-" `isPrefixOf` str = TkComment
classify "''" = TkSpecial
classify "'"  = TkSpecial
classify str@(c:_)
    | isSpace c = TkSpace
    | isDigit c = TkNumber
    | c `elem` special = TkSpecial
    | str `elem` glyphs = TkGlyph
    | all (`elem` symbols) str = TkOperator
    | c == '#' = TkCpp
    | c == '"' = TkString
    | c == '\'' = TkChar
classify str
    | str `elem` keywords = TkKeyword
    | isIdentifier str = TkIdentifier
    | otherwise = TkUnknown

keywords :: [String]
keywords =
    [ "as"
    , "case"
    , "class"
    , "data"
    , "default"
    , "deriving"
    , "do"
    , "else"
    , "hiding"
    , "if"
    , "import"
    , "in"
    , "infix"
    , "infixl"
    , "infixr"
    , "instance"
    , "let"
    , "module"
    , "newtype"
    , "of"
    , "qualified"
    , "then"
    , "type"
    , "where"
    , "forall"
    , "family"
    , "mdo"
    ]

glyphs :: [String]
glyphs =
    [ ".."
    , ":"
    , "::"
    , "="
    , "\\"
    , "|"
    , "<-"
    , "->"
    , "@"
    , "~"
    , "~#"
    , "=>"
    , "-"
    , "!"
    ]

special :: [Char]
special = "()[]{},;`"

-- TODO: Add support for any Unicode symbol or punctuation.
-- source: http://stackoverflow.com/questions/10548170/what-characters-are-permitted-for-haskell-operators
symbols :: [Char]
symbols = "!#$%&*+./<=>?@\\^|-~:"

isIdentifier :: String -> Bool
isIdentifier (s:str)
    | (isLower' s || isUpper s) && all isAlphaNum' str = True
  where
    isLower' c = isLower c || c == '_'
    isAlphaNum' c = isAlphaNum c || c == '_' || c == '\''
isIdentifier _ = False
