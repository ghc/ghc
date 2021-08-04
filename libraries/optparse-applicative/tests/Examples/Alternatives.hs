module Examples.Alternatives where

import Options.Applicative

data Value = A | B
  deriving (Eq, Show)

values :: Parser [Value]
values = many $ a <|> b

a :: Parser Value
a = flag' A (short 'a')

b :: Parser Value
b = flag' B (short 'b')

opts :: ParserInfo [Value]
opts = info values idm
