import IO
import IOExts
import Foreign
import CForeign

main = withCStringLen "hello world\n" $ \(ptr,len) -> hPutBuf stdout ptr len
