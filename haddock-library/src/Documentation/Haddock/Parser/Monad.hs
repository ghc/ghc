{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Documentation.Haddock.Parser.Monad where

import qualified Text.Parsec.Char as Parsec
import qualified Text.Parsec as Parsec

import qualified Data.Text as T
import           Data.Text                   ( Text )

import           Data.String                 ( IsString(..) )
import           Data.Bits                   ( Bits(..) )
import           Data.Char                   ( ord )
import           Data.List                   ( foldl' )
import           Control.Applicative as App

import           Documentation.Haddock.Types ( Version )

newtype ParserState = ParserState {
  parserStateSince :: Maybe Version
} deriving (Eq, Show)

initialParserState :: ParserState
initialParserState = ParserState Nothing

setSince :: Version -> Parser ()
setSince since = Parsec.modifyState (\st -> st {parserStateSince = Just since})

type Parser = Parsec.Parsec Text ParserState

instance (a ~ Text) => IsString (Parser a) where
  fromString = fmap T.pack . Parsec.string

parseOnly :: Parser a -> Text -> Either String (ParserState, a)
parseOnly p t = case Parsec.runParser p' initialParserState "<haddock>" t of
                  Left e -> Left (show e)
                  Right (x,s) -> Right (s,x)
  where p' = (,) <$> p <*> Parsec.getState

-- | Always succeeds, but returns 'Nothing' if at the end of input. Does not
-- consume input.
peekChar :: Parser (Maybe Char)
peekChar = Parsec.optionMaybe . Parsec.try . Parsec.lookAhead $ Parsec.anyChar

-- | Fails if at the end of input. Does not consume input.
peekChar' :: Parser Char
peekChar' = Parsec.lookAhead Parsec.anyChar 

-- | Parses the given string. Returns the parsed string.
string :: Text -> Parser Text
string t = Parsec.string (T.unpack t) *> App.pure t

-- | Scan the input text, accumulating characters as long as the scanning
-- function returns true.
scan :: (s -> Char -> Maybe s) -- ^ scan function
     -> s                      -- ^ initial state
     -> Parser Text 
scan f = fmap T.pack . go
  where go s1 = do { cOpt <- peekChar
                   ; case cOpt >>= f s1 of
                       Nothing -> pure ""
                       Just s2 -> (:) <$> Parsec.anyChar <*> go s2
                   }

-- | Apply a parser for a character zero or more times and collect the result in
-- a string.
takeWhile :: Parser Char -> Parser Text
takeWhile = fmap T.pack . Parsec.many

-- | Apply a parser for a character one or more times and collect the result in
-- a string.
takeWhile1 :: Parser Char -> Parser Text
takeWhile1 =  fmap T.pack . Parsec.many1

-- | Parse a decimal number.
decimal :: Integral a => Parser a
decimal = foldl' step 0 `fmap` Parsec.many1 Parsec.digit
  where step a c = a * 10 + fromIntegral (ord c - 48)

-- | Parse a hexadecimal number.
hexadecimal :: (Integral a, Bits a) => Parser a
hexadecimal = foldl' step 0 `fmap` Parsec.many1 Parsec.hexDigit 
  where
  step a c | w >= 48 && w <= 57  = (a `shiftL` 4) .|. fromIntegral (w - 48)
           | w >= 97             = (a `shiftL` 4) .|. fromIntegral (w - 87)
           | otherwise           = (a `shiftL` 4) .|. fromIntegral (w - 55)
    where w = ord c
