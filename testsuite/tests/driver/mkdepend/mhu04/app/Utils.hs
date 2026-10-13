module Utils (shout) where

import Data.Char (toUpper)

shout :: String -> String
shout = map toUpper
