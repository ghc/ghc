module Haddock.Backends.Hyperlinker.Parser (parse) where

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
chunk = undefined

tag :: [String] -> [(Span, String)]
tag = undefined

tokenize :: [(Span, String)] -> [Token]
tokenize = undefined
