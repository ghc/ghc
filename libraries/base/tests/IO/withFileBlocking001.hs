-- | Test that withFileBlocking does report file not found exceptions.

import System.IO
import GHC.IO.Handle.FD

-- | Test that withFileBlocking does report file not found exceptions.
main :: IO ()
main =
  withFileBlocking "test.txt" ReadMode $ \h ->
    hGetContents' h >> pure ()
