-- | Test that withFileBlocking does not annotate exceptions of inner computations.

import System.IO
import GHC.IO.Handle.FD

main :: IO ()
main =
  withFileBlocking "test.txt" WriteMode $ \_ ->
    fail "test"
