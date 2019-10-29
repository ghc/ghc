{-# LANGUAGE ScopedTypeVariables #-}
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (mallocBytes, free)
import System.IO (hPutBuf, withBinaryFile, IOMode (WriteMode))

-- more than 2GiB
numBytes :: Int
numBytes = 2264375889

main :: IO ()
main = do
  (ptr :: Ptr ()) <- mallocBytes numBytes
  -- the next line produces the exception on macOS
  withBinaryFile "test.out" WriteMode (\h -> hPutBuf h ptr numBytes)
  free ptr

  -- Truncate file in case it doesn't get deleted
  writeFile "test.out" ""
