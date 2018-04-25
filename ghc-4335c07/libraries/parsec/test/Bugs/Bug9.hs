
module Bugs.Bug9 ( main ) where

import Control.Applicative ((<*), (<$>), (<$))
import Text.Parsec
import Text.Parsec.Language (haskellStyle)
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P

import Test.HUnit hiding ( Test )
import Test.Framework
import Test.Framework.Providers.HUnit

import Util

data Expr = Const Integer | Op Expr Expr
  deriving Show

main :: Test
main =
  testCase "Tracing of current position in error message (#9)"
  $ result @?= ["unexpected '>'","expecting operator or end of input"]

  where
    result :: [String]
    result = parseErrors parseTopLevel "4 >> 5"

-- Syntax analaysis

parseTopLevel :: Parser Expr
parseTopLevel = parseExpr <* eof

parseExpr :: Parser Expr
parseExpr = buildExpressionParser table (Const <$> integer)
  where
        table = [[ Infix (Op <$ reserved ">>>") AssocLeft ]]

        -- Lexical analysis

        lexer = P.makeTokenParser haskellStyle { P.reservedOpNames = [">>>"] }

        integer    = P.integer    lexer
        reserved   = P.reserved   lexer
        reservedOp = P.reservedOp lexer

