import Foreign
import Foreign.C

foreign import stdcall "&test" ptest :: FunPtr (CInt -> IO ())
foreign import stdcall "dynamic" ctest :: FunPtr (CInt -> IO ()) -> CInt -> IO ()

main = ctest ptest 3
