{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Documentation.Haddock.Parser.Util
-- Copyright   :  (c) Mateusz Kowalczyk 2013-2014,
--                    Simon Hengel      2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Various utility functions used by the parser.
module Documentation.Haddock.Parser.Util
  ( takeUntil
  , removeEscapes
  , makeLabeled
  , takeHorizontalSpace
  , skipHorizontalSpace
  ) where

import Control.Applicative
import Control.Monad (mfilter)
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Parsec as Parsec
import Prelude hiding (takeWhile)

import Documentation.Haddock.Parser.Monad

-- | Characters that count as horizontal space
horizontalSpace :: Char -> Bool
horizontalSpace c = isSpace c && c /= '\n'

-- | Skip and ignore leading horizontal space
skipHorizontalSpace :: Parser ()
skipHorizontalSpace = Parsec.skipMany (Parsec.satisfy horizontalSpace)

-- | Take leading horizontal space
takeHorizontalSpace :: Parser Text
takeHorizontalSpace = takeWhile horizontalSpace

makeLabeled :: (String -> Maybe String -> a) -> Text -> a
makeLabeled f input = case T.break isSpace $ removeEscapes $ T.strip input of
  (uri, "") -> f (T.unpack uri) Nothing
  (uri, label) -> f (T.unpack uri) (Just . T.unpack $ T.stripStart label)

-- | Remove escapes from given string.
--
-- Only do this if you do not process (read: parse) the input any further.
removeEscapes :: Text -> Text
removeEscapes = T.unfoldr go
  where
    go :: Text -> Maybe (Char, Text)
    go xs = case T.uncons xs of
      Just ('\\', ys) -> T.uncons ys
      unconsed -> unconsed

-- | Consume characters from the input up to and including the given pattern.
-- Return everything consumed except for the end pattern itself.
takeUntil :: Text -> Parser Text
takeUntil end_ = T.dropEnd (T.length end_) <$> requireEnd (scan p (False, end)) >>= gotSome
  where
    end = T.unpack end_

    p :: (Bool, String) -> Char -> Maybe (Bool, String)
    p acc c = case acc of
      (True, _) -> Just (False, end)
      (_, []) -> Nothing
      (_, x : xs) | x == c -> Just (False, xs)
      _ -> Just (c == '\\', end)

    requireEnd = mfilter (T.isSuffixOf end_)

    gotSome xs
      | T.null xs = fail "didn't get any content"
      | otherwise = return xs
