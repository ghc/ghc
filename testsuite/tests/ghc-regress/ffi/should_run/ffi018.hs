
import Data.Int

main :: IO ()
main = do x <- f
          print (x == 0x123456789ABCDEF0)

foreign import ccall "ffi018.h f"
    f :: IO Int64

