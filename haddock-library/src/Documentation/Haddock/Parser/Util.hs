{-# LANGUAGE CPP #-}
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
module Documentation.Haddock.Parser.Util (
  unsnoc
, strip
, takeUntil
, makeLabeled
, takeHorizontalSpace
, skipHorizontalSpace
) where

import           Control.Applicative
import           Control.Monad (mfilter)
import           Data.Attoparsec.ByteString.Char8 hiding (parse, take, endOfLine)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Prelude hiding (takeWhile)

#if MIN_VERSION_bytestring(0,10,2)
import           Data.ByteString.Char8 (unsnoc)
#else
unsnoc :: ByteString -> Maybe (ByteString, Char)
unsnoc bs
  | BS.null bs = Nothing
  | otherwise = Just (BS.init bs, BS.last bs)
#endif

-- | Remove all leading and trailing whitespace
strip :: String -> String
strip = (\f -> f . f) $ dropWhile isSpace . reverse

skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipWhile (`elem` " \t\f\v\r")

takeHorizontalSpace :: Parser BS.ByteString
takeHorizontalSpace = takeWhile (`elem` " \t\f\v\r")

makeLabeled :: (String -> Maybe String -> a) -> String -> a
makeLabeled f input = case break isSpace $ removeEscapes $ strip input of
  (uri, "")    -> f uri Nothing
  (uri, label) -> f uri (Just $ dropWhile isSpace label)
  where
    -- As we don't parse these any further, we don't do any processing to the
    -- string so we at least remove escape character here. Perhaps we should
    -- actually be parsing the label at the very least?
    removeEscapes "" = ""
    removeEscapes ('\\':'\\':xs) = '\\' : removeEscapes xs
    removeEscapes ('\\':xs) = removeEscapes xs
    removeEscapes (x:xs) = x : removeEscapes xs

takeUntil :: ByteString -> Parser ByteString
takeUntil end_ = dropEnd <$> requireEnd (scan (False, end) p) >>= gotSome
  where
    end = BS.unpack end_

    p :: (Bool, String) -> Char -> Maybe (Bool, String)
    p acc c = case acc of
      (True, _) -> Just (False, end)
      (_, []) -> Nothing
      (_, x:xs) | x == c -> Just (False, xs)
      _ -> Just (c == '\\', end)

    dropEnd = BS.reverse . BS.drop (length end) . BS.reverse
    requireEnd = mfilter (BS.isSuffixOf end_)

    gotSome xs
      | BS.null xs = fail "didn't get any content"
      | otherwise = return xs
