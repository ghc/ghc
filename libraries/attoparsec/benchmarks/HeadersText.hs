{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module HeadersText (headers) where

import Common (pathTo, rechunkT)
import Control.Applicative
import Criterion.Main (bench, bgroup, nf)
import Criterion.Types (Benchmark)
import Data.Char (isSpace)
import qualified Data.Attoparsec.Text as T
import qualified Data.Attoparsec.Text.Lazy as TL
import qualified Data.Text.IO as T

header = do
  name <- T.takeWhile1 (T.inClass "a-zA-Z0-9_-") <* T.char ':' <* T.skipSpace
  body <- (:) <$> bodyLine <*> many (T.takeWhile1 isSpace *> bodyLine)
  return (name, body)

bodyLine = T.takeTill (\c -> c == '\r' || c == '\n') <* T.endOfLine

requestLine =
    (,,) <$>
    (method <* T.skipSpace) <*>
    (T.takeTill isSpace <* T.skipSpace) <*>
    httpVersion
  where method = "GET" <|> "POST"

httpVersion = "HTTP/" *> ((,) <$> (int <* T.char '.') <*> int)

responseLine = (,,) <$>
               (httpVersion <* T.skipSpace) <*>
               (int <* T.skipSpace) <*>
               bodyLine

int :: T.Parser Int
int = T.decimal

request = (,) <$> (requestLine <* T.endOfLine) <*> many header

response = (,) <$> responseLine <*> many header

headers :: IO Benchmark
headers = do
  req <- T.readFile =<< pathTo "http-request.txt"
  resp <- T.readFile =<< pathTo "http-response.txt"
  let reql    = rechunkT 4 req
      respl   = rechunkT 4 resp
  return $ bgroup "headers" [
      bgroup "T" [
        bench "request" $ nf (T.parseOnly request) req
      , bench "response" $ nf (T.parseOnly response) resp
      ]
    , bgroup "TL" [
        bench "request" $ nf (TL.parse request) reql
      , bench "response" $ nf (TL.parse response) respl
      ]
    ]
