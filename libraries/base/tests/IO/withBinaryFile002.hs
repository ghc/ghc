-- | Test that withBinaryFile does not annotate exceptions of inner computations.

import System.IO

main :: IO ()
main =
  withBinaryFile "test.bin" WriteMode $ \_ ->
    fail "test"
