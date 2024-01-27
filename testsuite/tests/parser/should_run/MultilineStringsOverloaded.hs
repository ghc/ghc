{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.String (IsString (..))
import Data.Text (Text)

newtype Lines s = Lines [s]
  deriving (Show)

instance IsString s => IsString (Lines s) where
  fromString = Lines . map fromString . lines

lines0 :: Lines Text
lines0 =
  """
  this is
  a test
  with multiple lines
  """

main :: IO ()
main = print lines0
