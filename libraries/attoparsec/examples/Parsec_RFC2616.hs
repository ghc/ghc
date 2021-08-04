{-# LANGUAGE BangPatterns, CPP, FlexibleContexts #-}

module Main (main) where

import Control.Applicative
import Control.Exception (bracket)
import System.Environment (getArgs)
import System.IO (hClose, openFile, IOMode(ReadMode))
import Text.Parsec.Char (anyChar, char, satisfy, string)
import Text.Parsec.Combinator (many1, manyTill, skipMany1)
import Text.Parsec.Prim hiding (many, token, (<|>))
import qualified Data.IntSet as S

#if 1
import Text.Parsec.ByteString.Lazy (Parser, parseFromFile)
import qualified Data.ByteString.Lazy as B
#else
import Text.Parsec.ByteString (Parser, parseFromFile)
import qualified Data.ByteString as B
#endif

token :: Stream s m Char => ParsecT s u m Char
token = satisfy $ \c -> S.notMember (fromEnum c) set
  where set = S.fromList . map fromEnum $ ['\0'..'\31'] ++ "()<>@,;:\\\"/[]?={} \t" ++ ['\128'..'\255']

isHorizontalSpace :: Char -> Bool
isHorizontalSpace c = c == ' ' || c == '\t'

skipHSpaces :: Stream s m Char => ParsecT s u m ()
skipHSpaces = skipMany1 (satisfy isHorizontalSpace)

data Request = Request {
      _requestMethod   :: String
    , _requestUri      :: String
    , _requestProtocol :: String
    } deriving (Eq, Ord, Show)

requestLine :: Stream s m Char => ParsecT s u m Request
requestLine = do
  method <- many1 token <* skipHSpaces
  uri <- many1 (satisfy (not . isHorizontalSpace)) <* skipHSpaces <* string "HTTP/"
  proto <- many httpVersion <* endOfLine
  return $! Request method uri proto
 where
  httpVersion = satisfy $ \c -> c == '1' || c == '0' || c == '.'

endOfLine :: Stream s m Char => ParsecT s u m ()
endOfLine = (string "\r\n" *> pure ()) <|> (char '\n' *> pure ())

data Header = Header {
      _headerName  :: String
    , _headerValue :: [String]
    } deriving (Eq, Ord, Show)

messageHeader :: Stream s m Char => ParsecT s u m Header
messageHeader = do
  header <- many1 token <* char ':' <* skipHSpaces
  body <- manyTill anyChar endOfLine
  conts <- many $ skipHSpaces *> manyTill anyChar endOfLine
  return $! Header header (body:conts)

request :: Stream s m Char => ParsecT s u m (Request, [Header])
request = (,) <$> requestLine <*> many messageHeader <* endOfLine

listy :: FilePath -> IO ()
listy arg = do
  r <- parseFromFile (many request) arg
  case r of
    Left err -> putStrLn $ arg ++ ": " ++ show err
    Right rs -> print (length rs)

chunky :: FilePath -> IO ()
chunky arg = bracket (openFile arg ReadMode) hClose $ \h ->
               loop (0::Int) =<< B.hGetContents h
 where
  loop !n bs
      | B.null bs = print n
      | otherwise = case parse myReq arg bs of
                      Left err      -> putStrLn $ arg ++ ": " ++ show err
                      Right (r,bs') -> loop (n+1) bs'
  myReq :: Parser ((Request, [Header]), B.ByteString)
  myReq = liftA2 (,) request getInput

main :: IO ()
main = mapM_ f =<< getArgs
  where
    --f = listy
    f = chunky
