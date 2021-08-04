import Control.Applicative ((<|>), many)
import Control.Monad (forM_)
import System.Environment (getArgs)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import qualified Text.Parsec as P
import qualified Text.Parsec.ByteString as P

attoparsec = do
  args <- getArgs
  forM_ args $ \arg -> do
    input <- B.readFile arg
    case A.parse p input `A.feed` B.empty of
      A.Done _ xs -> print (length xs)
      what        -> print what
 where
  slow = many (A.many1 A.letter_ascii <|> A.many1 A.digit)
  fast = many (A.takeWhile1 isLetter <|> A.takeWhile1 isDigit)
  isDigit c  = c >= '0' && c <= '9'
  isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  p = fast

parsec = do
  args <- getArgs
  forM_ args $ \arg -> do
    input <- readFile arg
    case P.parse (P.many (P.many1 P.letter P.<|> P.many1 P.digit)) "" input of
      Left err -> print err
      Right xs -> print (length xs)

main = attoparsec
