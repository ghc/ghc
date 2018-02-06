{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Documentation.Haddock.Parser.Monad (
  module Documentation.Haddock.Parser.Monad
, Attoparsec.isDigit
, Attoparsec.isDigit_w8
, Attoparsec.isAlpha_iso8859_15
, Attoparsec.isAlpha_ascii
, Attoparsec.isSpace
, Attoparsec.isSpace_w8
, Attoparsec.inClass
, Attoparsec.notInClass
, Attoparsec.isEndOfLine
, Attoparsec.isHorizontalSpace
, Attoparsec.choice
, Attoparsec.count
, Attoparsec.option
, Attoparsec.many'
, Attoparsec.many1
, Attoparsec.many1'
, Attoparsec.manyTill
, Attoparsec.manyTill'
, Attoparsec.sepBy
, Attoparsec.sepBy'
, Attoparsec.sepBy1
, Attoparsec.sepBy1'
, Attoparsec.skipMany
, Attoparsec.skipMany1
, Attoparsec.eitherP
) where

import           Control.Applicative
import           Control.Monad
import           Data.String
import           Data.ByteString (ByteString, length)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import qualified Data.Attoparsec.Combinator as Attoparsec
import           Control.Monad.Trans.State
import qualified Control.Monad.Trans.Class as Trans
import           Data.Word
import           Data.Bits
import           Data.Tuple

import           Documentation.Haddock.Types (Version)
import           Documentation.Haddock.Utf8  (encodeUtf8, decodeUtf8)

newtype ParserState = ParserState {
  parserStateSince :: Maybe Version
} deriving (Eq, Show)

initialParserState :: ParserState
initialParserState = ParserState Nothing

newtype Parser a = Parser (StateT ParserState Attoparsec.Parser a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

instance (a ~ ByteString) => IsString (Parser a) where
  fromString = lift . fromString

parseOnly :: Parser a -> ByteString -> Either String (ParserState, a)
parseOnly (Parser p) = fmap swap . Attoparsec.parseOnly (runStateT p initialParserState)

lift :: Attoparsec.Parser a -> Parser a
lift = Parser . Trans.lift

setParserState :: ParserState -> Parser ()
setParserState = Parser . put

setSince :: Version -> Parser ()
setSince since = Parser $ modify (\st -> st {parserStateSince = Just since})

char :: Char -> Parser Char
char = lift . Attoparsec.char

char8 :: Char -> Parser Word8
char8 = lift . Attoparsec.char8

-- | Peek a unicode character and return the number of bytes that it took up
peekUnicode :: Parser (Char, Int)
peekUnicode = lift $ Attoparsec.lookAhead $ do
  
  -- attoparsec's take fails on shorter inputs rather than truncate
  bs <- Attoparsec.choice (map Attoparsec.take [4,3,2,1])
  
  let c = head . decodeUtf8 $ bs
      n = Data.ByteString.length . encodeUtf8 $ [c]
  pure (c, fromIntegral n)

-- | Like 'satisfy', but consuming a unicode character
satisfyUnicode :: (Char -> Bool) -> Parser Char
satisfyUnicode predicate = do
  (c,n) <- peekUnicode
  if predicate c
    then Documentation.Haddock.Parser.Monad.take n *> pure c
    else fail "satsifyUnicode"

anyChar :: Parser Char
anyChar = lift Attoparsec.anyChar

notChar :: Char -> Parser Char
notChar = lift . Attoparsec.notChar

satisfy :: (Char -> Bool) -> Parser Char
satisfy = lift . Attoparsec.satisfy

peekChar :: Parser (Maybe Char)
peekChar = lift Attoparsec.peekChar

peekChar' :: Parser Char
peekChar' = lift Attoparsec.peekChar'

digit :: Parser Char
digit = lift Attoparsec.digit

letter_iso8859_15 :: Parser Char
letter_iso8859_15 = lift Attoparsec.letter_iso8859_15

letter_ascii :: Parser Char
letter_ascii = lift Attoparsec.letter_ascii

space :: Parser Char
space = lift Attoparsec.space

string :: ByteString -> Parser ByteString
string = lift . Attoparsec.string

stringCI :: ByteString -> Parser ByteString
stringCI = lift . Attoparsec.stringCI

skipSpace :: Parser ()
skipSpace = lift Attoparsec.skipSpace

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile = lift . Attoparsec.skipWhile

take :: Int -> Parser ByteString
take = lift . Attoparsec.take

scan :: s -> (s -> Char -> Maybe s) -> Parser ByteString
scan s = lift . Attoparsec.scan s

takeWhile :: (Char -> Bool) -> Parser ByteString
takeWhile = lift . Attoparsec.takeWhile

takeWhile1 :: (Char -> Bool) -> Parser ByteString
takeWhile1 = lift . Attoparsec.takeWhile1

takeTill :: (Char -> Bool) -> Parser ByteString
takeTill = lift . Attoparsec.takeTill

takeByteString :: Parser ByteString
takeByteString = lift Attoparsec.takeByteString

takeLazyByteString :: Parser LB.ByteString
takeLazyByteString = lift Attoparsec.takeLazyByteString

endOfLine :: Parser ()
endOfLine = lift Attoparsec.endOfLine

decimal :: Integral a => Parser a
decimal = lift Attoparsec.decimal

hexadecimal :: (Integral a, Bits a) => Parser a
hexadecimal = lift Attoparsec.hexadecimal

endOfInput :: Parser ()
endOfInput = lift Attoparsec.endOfInput

atEnd :: Parser Bool
atEnd = lift Attoparsec.atEnd
