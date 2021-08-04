{-# LANGUAGE OverloadedStrings #-}

module Links (links) where

import Control.Applicative
import Control.DeepSeq (NFData(..))
import Criterion.Main (Benchmark, bench, nf)
import Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString.Char8 as A8
import Data.ByteString.Char8 as B8

data Link = Link {
      linkURL :: ByteString
    , linkParams :: [(ByteString, ByteString)]
    } deriving (Eq, Show)

instance NFData Link where
    rnf l = rnf (linkURL l) `seq` rnf (linkParams l)

link :: Parser Link
link = Link <$> url <*> many (char8 ';' *> skipSpace *> param)
  where url = char8 '<' *> A8.takeTill (=='>') <* char8 '>' <* skipSpace

param :: Parser (ByteString, ByteString)
param = do
  name <- paramName
  skipSpace *> "=" *> skipSpace
  c <- peekChar'
  let isTokenChar = A.inClass "!#$%&'()*+./0-9:<=>?@a-zA-Z[]^_`{|}~-"
  val <- case c of
           '"' -> quotedString
           _   -> A.takeWhile isTokenChar
  skipSpace
  return (name, val)

data Quot = Literal | Backslash

quotedString :: Parser ByteString
quotedString = char '"' *> (fixup <$> body) <* char '"'
  where body = A8.scan Literal $ \s c ->
          case (s,c) of
            (Literal,  '\\') -> backslash
            (Literal,  '"')  -> Nothing
            _                -> literal
        literal   = Just Literal
        backslash = Just Backslash
        fixup = B8.pack . go . B8.unpack
          where go ('\\' : x@'\\' : xs) = x : go xs
                go ('\\' : x@'"' : xs)  = x : go xs
                go (x : xs)             = x : go xs
                go xs                   = xs

paramName :: Parser ByteString
paramName = do
  name <- A.takeWhile1 $ A.inClass "a-zA-Z0-9!#$&+-.^_`|~"
  c <- peekChar
  return $ case c of
             Just '*' -> B8.snoc name '*'
             _        -> name

links :: Benchmark
links = bench "links" $ nf (A.parseOnly link) lnk
  where lnk = "<https://api.github.com/search/code?q=addClass+user%3Amozilla&page=2>; rel=\"next\", <https://api.github.com/search/code?q=addClass+user%3Amozilla&page=34>; rel=\"last\""
