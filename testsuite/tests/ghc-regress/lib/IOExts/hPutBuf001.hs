import IO
import System.IO
import Foreign
import CForeign

main = do
  hSetBinaryMode stdout True
  withCStringLen "hello world\n" $ \(ptr,len) -> hPutBuf stdout ptr len
