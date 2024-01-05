-- | Test that withFile does not annotate exceptions of inner computations.

import System.IO

main :: IO ()
main =
  withFile "test.txt" WriteMode $ \_ ->
    fail "test"
