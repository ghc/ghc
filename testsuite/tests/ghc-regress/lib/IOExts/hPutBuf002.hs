import IO
import IOExts
import Foreign
import CForeign

-- !!! this test failed to write anything in GHC 5.00.2
main = do
  h <- openFile "hPutBuf002.out" ReadWriteMode
  withCStringLen "hello world\n" $ \(ptr,len) -> hPutBuf h ptr len
  hFileSize h >>= print
