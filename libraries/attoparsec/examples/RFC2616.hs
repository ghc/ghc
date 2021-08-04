{-# LANGUAGE OverloadedStrings #-}

module RFC2616
    (
      Header(..)
    , Request(..)
    , Response(..)
    , request
    , response
    ) where

import Control.Applicative
import Data.Attoparsec.ByteString as P
import Data.Attoparsec.ByteString.Char8 (char8, endOfLine, isDigit_w8)
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Data.Attoparsec.ByteString.Char8 (isEndOfLine, isHorizontalSpace)

isToken :: Word8 -> Bool
isToken w = w <= 127 && notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w

skipSpaces :: Parser ()
skipSpaces = satisfy isHorizontalSpace *> skipWhile isHorizontalSpace

data Request = Request {
      requestMethod  :: ByteString
    , requestUri     :: ByteString
    , requestVersion :: ByteString
    } deriving (Eq, Ord, Show)

httpVersion :: Parser ByteString
httpVersion = "HTTP/" *> P.takeWhile (\c -> isDigit_w8 c || c == 46)

requestLine :: Parser Request
requestLine = Request <$> (takeWhile1 isToken <* char8 ' ')
                      <*> (takeWhile1 (/=32) <* char8 ' ')
                      <*> (httpVersion <* endOfLine)

data Header = Header {
      headerName  :: ByteString
    , headerValue :: [ByteString]
    } deriving (Eq, Ord, Show)

messageHeader :: Parser Header
messageHeader = Header
  <$> (P.takeWhile isToken <* char8 ':' <* skipWhile isHorizontalSpace)
  <*> ((:) <$> (takeTill isEndOfLine <* endOfLine)
           <*> (many $ skipSpaces *> takeTill isEndOfLine <* endOfLine))

request :: Parser (Request, [Header])
request = (,) <$> requestLine <*> many messageHeader <* endOfLine

data Response = Response {
      responseVersion :: ByteString
    , responseCode    :: ByteString
    , responseMsg     :: ByteString
    } deriving (Eq, Ord, Show)

responseLine :: Parser Response
responseLine = Response <$> (httpVersion <* char8 ' ')
                        <*> (P.takeWhile isDigit_w8 <* char8 ' ')
                        <*> (takeTill isEndOfLine <* endOfLine)

response :: Parser (Response, [Header])
response = (,) <$> responseLine <*> many messageHeader <* endOfLine
