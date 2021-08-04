{-# LANGUAGE OverloadedStrings #-}

-- This benchmark reveals a huge performance regression that showed up
-- under GHC 7.8.1 (https://github.com/bos/attoparsec/issues/56).
--
-- With GHC 7.6.3 and older, this program runs in 0.04 seconds.  Under
-- GHC 7.8.1 with (<|>) inlined, time jumps to 12 seconds!

import Control.Applicative
import Data.Text (Text)
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T

testParser :: Text -> Either String Int
testParser f = fmap length -- avoid printing out the entire matched list
        . A.parseOnly (many ((() <$ A.string "b") <|> (() <$ A.anyChar)))
        $ f

main :: IO ()
main = print . testParser $ T.replicate 50000 "a"
