-- | Test that withFile does report file not found exceptions.

import System.IO

main :: IO ()
main =
  withFile "test.txt" ReadMode $ \h ->
    hGetContents' h >> pure ()
