module Haddock.Backends.Hyperlinker.Parser (parse) where

import Data.Char
import Data.List

data Token = Token
    { tkType :: TokenType
    , tkValue :: String
    , tkSpan :: Span
    }

data Position = Position
    { posRow :: !Int
    , posCol :: !Int
    }

data Span = Span
    { spStart :: Position
    , spEnd :: Position
    }

data TokenType
    = Identifier
    | Comment
    | Whitespace
    | Operator
    | Symbol

parse :: String -> [Token]
parse = tokenize . tag . chunk

chunk :: String -> [String]
chunk [] = []
chunk str@(c:_)
    | isSpace c = chunk' $ span isSpace str
chunk str
    | "--" `isPrefixOf` str = chunk' $ span (not . (== '\n')) str
    | "{-" `isPrefixOf` str = chunk' $ chunkComment 0 str
    | otherwise = chunk' $ head $ lex str

chunk' :: (String, String) -> [String]
chunk' (c, rest) = c:(chunk rest)

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

tag :: [String] -> [(Span, String)]
tag =
    reverse . snd . foldl aux (Position 1 1, [])
  where
    aux (pos, cs) c =
        let pos' = if c == "\n"
                   then pos { posRow = posRow pos + 1, posCol = 1 }
                   else pos { posCol = posCol pos + length c }
        in (pos', (Span pos pos', c):cs)

tokenize :: [(Span, String)] -> [Token]
tokenize = undefined
