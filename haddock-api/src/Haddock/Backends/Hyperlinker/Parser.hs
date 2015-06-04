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
