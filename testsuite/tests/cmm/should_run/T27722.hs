import Control.Monad
import Foreign.Ptr
import System.Exit

foreign import ccall "&zork_info" zork_info :: Ptr ()

main :: IO ()
main = when (zork_info == nullPtr) exitFailure
