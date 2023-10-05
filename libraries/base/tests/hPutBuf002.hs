import System.IO
import Foreign
import Foreign.C

-- !!! this test failed to write anything in GHC 5.00.2
main = do
  h <- openBinaryFile "hPutBuf002.out" ReadWriteMode
  withCStringLen "hello world\n" $ \(ptr,len) -> hPutBuf h ptr len
  hFileSize h >>= print
