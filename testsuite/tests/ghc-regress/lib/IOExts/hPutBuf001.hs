import System.IO
import Foreign
import Foreign.C

main = do
  hSetBinaryMode stdout True
  withCStringLen "hello world\n" $ \(ptr,len) -> hPutBuf stdout ptr len
