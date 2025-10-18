module Example.Text (fromString) where

import Data.Text (Text, pack)

fromString :: String -> Text
fromString = pack
