import Foreign
import Foreign.C

foreign import ccall "&test" ptest :: FunPtr (CInt -> IO ())
foreign import ccall "dynamic" ctest :: FunPtr (CInt -> IO ()) -> CInt -> IO ()

main = ctest ptest 3
