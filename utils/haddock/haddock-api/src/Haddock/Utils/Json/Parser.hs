-- | Json "Parsec" parser, based on
-- [json](https://hackage.haskell.org/package/json) package.
module Haddock.Utils.Json.Parser
  ( parseJSONValue
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import Data.Char (isHexDigit)
import Data.Functor (($>))
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Read as TR
import Numeric (readHex)
import Text.Parsec.Text (Parser)
import Text.ParserCombinators.Parsec ((<?>))
import qualified Text.ParserCombinators.Parsec as Parsec
import Prelude hiding (null)

import Haddock.Utils.Json.Types hiding (object)

parseJSONValue :: Parser Value
parseJSONValue = Parsec.spaces *> parseValue

tok :: Parser a -> Parser a
tok p = p <* Parsec.spaces

parseValue :: Parser Value
parseValue =
  parseNull
    <|> Bool
    <$> parseBoolean
      <|> Array
    <$> parseArray
      <|> String
    <$> parseString
      <|> Object
    <$> parseObject
      <|> Number
    <$> parseNumber
      <?> "JSON value"

parseNull :: Parser Value
parseNull =
  tok $
    Parsec.string "null"
      $> Null

parseBoolean :: Parser Bool
parseBoolean =
  tok $
    Parsec.string "true"
      $> True
        <|> Parsec.string "false"
      $> False

parseArray :: Parser [Value]
parseArray =
  Parsec.between
    (tok (Parsec.char '['))
    (tok (Parsec.char ']'))
    (parseValue `Parsec.sepBy` tok (Parsec.char ','))

parseString :: Parser Text
parseString =
  TL.toStrict . TB.toLazyText <$>
    Parsec.between
      (tok (Parsec.char '"'))
      (tok (Parsec.char '"'))
      (manyCharText mempty)
  where
    manyCharText acc =
      (char >>= \c -> manyCharText (acc <> TB.singleton c))
        <|> pure acc

    char =
      (Parsec.char '\\' >> escapedChar)
        <|> Parsec.satisfy (\x -> x /= '"' && x /= '\\')

    escapedChar =
      Parsec.char '"'
        $> '"'
          <|> Parsec.char '\\'
        $> '\\'
          <|> Parsec.char '/'
        $> '/'
          <|> Parsec.char 'b'
        $> '\b'
          <|> Parsec.char 'f'
        $> '\f'
          <|> Parsec.char 'n'
        $> '\n'
          <|> Parsec.char 'r'
        $> '\r'
          <|> Parsec.char 't'
        $> '\t'
          <|> Parsec.char 'u'
        *> uni
          <?> "escape character"

    uni = check =<< Parsec.count 4 (Parsec.satisfy isHexDigit)
      where
        check :: Enum a => String -> Parser a
        check x = do
          code <- parseHex x
          if code <= max_char
            then pure (toEnum code)
            else mzero
        parseHex :: String -> Parser Int
        parseHex c =
          case List.uncons (readHex c) of
            Nothing -> mzero
            Just (result, _) -> pure $ fst result
        max_char :: Int
        max_char = fromEnum (maxBound :: Char)

parseObject :: Parser Object
parseObject =
  Parsec.between
    (tok (Parsec.char '{'))
    (tok (Parsec.char '}'))
    (field `Parsec.sepBy` tok (Parsec.char ','))
  where
    field :: Parser (Text, Value)
    field =
      (,)
        <$> parseString
        <* tok (Parsec.char ':')
        <*> parseValue

parseNumber :: Parser Double
parseNumber = tok $ do
  s <- Parsec.getInput
  case TR.signed TR.double s of
    Right (n, s') -> Parsec.setInput s' $> n
    Left _ -> mzero
