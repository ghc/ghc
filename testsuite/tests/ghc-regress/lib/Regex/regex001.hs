module Main where

import Control.Exception (try)
import Text.Regex.Posix

-- caused GHC 6.0 to crash, due to regfree'ing the regex after a
-- failed regcomp.
main = sequence_ [ try $ regcomp "[[[" 0 | _ <- [1..10000] ]
