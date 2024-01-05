-- | Test that withBinaryFile does report file not found exceptions.

import System.IO

main :: IO ()
main =
  withBinaryFile "test.bin" ReadMode $ \h ->
    hGetContents' h >> pure ()
