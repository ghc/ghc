{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Documentation.Haddock.Parser.Monad
-- Copyright   :  (c) Alec Theriault 2018-2019,
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Defines the Parsec monad over which all parsing is done and also provides
-- more efficient versions of the usual parsec combinator functions (but
-- specialized to 'Text').
module Documentation.Haddock.Parser.Monad where

import Text.Parsec
  ( State (..)
  , getParserState
  , setParserState
  )
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Char as Parsec
import Text.Parsec.Pos (updatePosChar)

import Data.Text (Text)
import qualified Data.Text as T

import Control.Applicative as App
import Control.Monad (mfilter)
import Data.Bits (Bits (..))
import Data.Char (ord)
import Data.List (foldl')
import Data.String (IsString (..))

import Documentation.Haddock.Types (MetaSince (..))

import CompatPrelude
import Prelude hiding (takeWhile)

-- | The only bit of information we really care about trudging along with us
-- through parsing is the version attached to a @\@since@ annotation - if
-- the doc even contained one.
newtype ParserState = ParserState
  { parserStateSince :: Maybe MetaSince
  }
  deriving (Eq, Show)

initialParserState :: ParserState
initialParserState = ParserState Nothing

setSince :: MetaSince -> Parser ()
setSince since = Parsec.modifyState (\st -> st{parserStateSince = Just since})

type Parser = Parsec.Parsec Text ParserState

instance a ~ Text => IsString (Parser a) where
  fromString = fmap T.pack . Parsec.string

parseOnly :: Parser a -> Text -> Either String (ParserState, a)
parseOnly p t = case Parsec.runParser p' initialParserState "<haddock>" t of
  Left e -> Left (show e)
  Right (x, s) -> Right (s, x)
  where
    p' = (,) <$> p <*> Parsec.getState

-- | Always succeeds, but returns 'Nothing' if at the end of input. Does not
-- consume input.
--
-- Equivalent to @Parsec.optionMaybe . Parsec.lookAhead $ Parsec.anyChar@, but
-- more efficient.
peekChar :: Parser (Maybe Char)
peekChar = headOpt . stateInput <$> getParserState
  where
    headOpt t
      | T.null t = Nothing
      | otherwise = Just (T.head t)
{-# INLINE peekChar #-}

-- | Fails if at the end of input. Does not consume input.
--
-- Equivalent to @Parsec.lookAhead Parsec.anyChar@, but more efficient.
peekChar' :: Parser Char
peekChar' = headFail . stateInput =<< getParserState
  where
    headFail t
      | T.null t = Parsec.parserFail "peekChar': reached EOF"
      | otherwise = App.pure (T.head t)
{-# INLINE peekChar' #-}

-- | Parses the given string. Returns the parsed string.
--
-- Equivalent to @Parsec.string (T.unpack t) $> t@, but more efficient.
string :: Text -> Parser Text
string t = do
  s@State{stateInput = inp, statePos = pos} <- getParserState
  case T.stripPrefix t inp of
    Nothing -> Parsec.parserFail "string: Failed to match the input string"
    Just inp' ->
      let pos' = T.foldl updatePosChar pos t
          s' = s{stateInput = inp', statePos = pos'}
       in setParserState s' $> t

-- | Keep matching characters as long as the predicate function holds (and
-- return them).
--
-- Equivalent to @fmap T.pack . Parsec.many@, but more efficient.
takeWhile :: (Char -> Bool) -> Parser Text
takeWhile f = do
  s@State{stateInput = inp, statePos = pos} <- getParserState
  let (t, inp') = T.span f inp
      pos' = T.foldl updatePosChar pos t
      s' = s{stateInput = inp', statePos = pos'}
  setParserState s' $> t

-- | Like 'takeWhile', but fails if no characters matched.
--
-- Equivalent to @fmap T.pack . Parsec.many1@, but more efficient.
takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 = mfilter (not . T.null) . takeWhile

-- | Scan the input text, accumulating characters as long as the scanning
-- function returns true.
scan
  :: (s -> Char -> Maybe s)
  -- ^ scan function
  -> s
  -- ^ initial state
  -> Parser Text
scan f st = do
  s@State{stateInput = inp, statePos = pos} <- getParserState
  go inp st pos 0 $ \inp' pos' n ->
    let s' = s{Parsec.stateInput = inp', Parsec.statePos = pos'}
     in setParserState s' $> T.take n inp
  where
    go inp s !pos !n cont =
      case T.uncons inp of
        Nothing -> cont inp pos n -- ran out of input
        Just (c, inp') ->
          case f s c of
            Nothing -> cont inp pos n -- scan function failed
            Just s' -> go inp' s' (updatePosChar pos c) (n + 1) cont

-- | Parse a decimal number.
decimal :: Integral a => Parser a
decimal = foldl' step 0 `fmap` Parsec.many1 Parsec.digit
  where
    step a c = a * 10 + fromIntegral (ord c - 48)

-- | Parse a hexadecimal number.
hexadecimal :: (Integral a, Bits a) => Parser a
hexadecimal = foldl' step 0 `fmap` Parsec.many1 Parsec.hexDigit
  where
    step a c
      | w >= 48 && w <= 57 = (a `shiftL` 4) .|. fromIntegral (w - 48)
      | w >= 97 = (a `shiftL` 4) .|. fromIntegral (w - 87)
      | otherwise = (a `shiftL` 4) .|. fromIntegral (w - 55)
      where
        w = ord c
