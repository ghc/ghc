module Hospital where

import ShortText

foo :: Int -> String
foo i = toString (ShortText (show i))
