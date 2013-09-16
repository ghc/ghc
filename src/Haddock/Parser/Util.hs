module Haddock.Parser.Util where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

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
