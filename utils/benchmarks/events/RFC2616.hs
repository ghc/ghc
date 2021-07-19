{-# LANGUAGE OverloadedStrings #-}

module RFC2616
    (
      Header(..)
    , Request(..)
    , Response(..)
    , isToken
    , messageHeader
    , request
    , requestLine
    , response
    , responseLine
    , lowerHeader
    , lookupHeader
    ) where

import Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Char8 as P8
import Data.Attoparsec.ByteString.Char8 (char8, endOfLine, isDigit_w8)
import Data.Word (Word8)
import qualified Data.ByteString.Char8 as B hiding (map)
import qualified Data.ByteString as B (map)

isToken :: Word8 -> Bool
isToken w = w <= 127 && notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w

skipSpaces :: Parser ()
skipSpaces = satisfy P8.isHorizontalSpace *> skipWhile P8.isHorizontalSpace

data Request = Request {
      requestMethod  :: !B.ByteString
    , requestUri     :: !B.ByteString
    , requestVersion :: !B.ByteString
    } deriving (Eq, Ord, Show)

httpVersion :: Parser B.ByteString
httpVersion = string "HTTP/" *> P.takeWhile (\c -> isDigit_w8 c || c == 46)

requestLine :: Parser Request
requestLine = do
  method <- P.takeWhile1 isToken <* char8 ' '
  uri <- P.takeWhile1 (/=32) <* char8 ' '
  version <- httpVersion <* endOfLine
  return $! Request method uri version

data Header = Header {
      headerName  :: !B.ByteString
    , headerValue :: [B.ByteString]
    } deriving (Eq, Ord, Show)

messageHeader :: Parser Header
messageHeader = do
  header <- P.takeWhile isToken <* char8 ':' <* P.skipWhile P8.isHorizontalSpace
  body <- P.takeTill P8.isEndOfLine <* endOfLine
  bodies <- P.many' $ skipSpaces *> P.takeTill P8.isEndOfLine <* endOfLine
  return $! Header header (body:bodies)

request :: Parser (Request, [Header])
request = (,) <$> requestLine <*> P.many' messageHeader <* endOfLine

data Response = Response {
      responseVersion :: !B.ByteString
    , responseCode    :: !B.ByteString
    , responseMsg     :: !B.ByteString
    } deriving (Eq, Ord, Show)

responseLine :: Parser Response
responseLine = do
  version <- httpVersion <* char8 ' '
  code <- P.takeWhile isDigit_w8 <* char8 ' '
  msg <- P.takeTill P8.isEndOfLine <* endOfLine
  return $! Response version code msg

response :: Parser (Response, [Header])
response = (,) <$> responseLine <*> P.many' messageHeader <* endOfLine

lowerHeader :: Header -> Header
lowerHeader (Header n v) = Header (B.map toLower n) (map (B.map toLower) v)
  where toLower w | w >= 65 && w <= 90 = w + 32
                  | otherwise          = w

lookupHeader :: B.ByteString -> [Header] -> [B.ByteString]
lookupHeader k = go
  where
    go (Header n v:hs)
      | k == n    = v
      | otherwise = go hs
    go _          = []
