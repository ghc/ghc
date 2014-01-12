module FileIO where
import System.IO
import Foreign
import Foreign.C


foreign import ccall safe "fileio.h c_file_getresult" 
  c_file_getresult :: CInt -> IO CInt

