module Main where

import Control.Exception
import Text.Regex.Posix

-- caused GHC 6.0 to crash, due to regfree'ing the regex after a
-- failed regcomp.
main = sequence_
           [ try ("abc" =~~ "[[[" :: IO Bool) :: IO (Either IOException Bool)
           | _ <- [1..10000] ]

