import Foreign
import Foreign.C

foreign import ccall "test" ctest :: CInt -> IO ()

main = ctest 3
