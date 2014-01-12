import Foreign
import Foreign.C

foreign import stdcall "test" ctest :: CInt -> IO ()

main = ctest 3
