import Data.Int
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "&" foo :: Ptr Int64

main :: IO ()
main = peek foo >>= print
