import Data.Bits
import Numeric.Natural
import GHC.Exception.Type
import Control.Exception

main :: IO ()
main = do
   test ((42 `shiftR` (-1)) :: Integer)
   test ((42 `shiftL` (-1)) :: Integer)
   test ((42 `shiftR` (-1)) :: Natural)
   test ((42 `shiftL` (-1)) :: Natural)
   test ((42 `shiftR` (-1)) :: Word)
   test ((42 `shiftL` (-1)) :: Word)
   test ((42 `shiftR` (-1)) :: Int)
   test ((42 `shiftL` (-1)) :: Int)

   test ((42 `unsafeShiftR` 2) :: Integer)
   test ((42 `unsafeShiftL` 2) :: Integer)
   test ((42 `unsafeShiftR` 2) :: Natural)
   test ((42 `unsafeShiftL` 2) :: Natural)
   test ((42 `unsafeShiftR` 2) :: Word)
   test ((42 `unsafeShiftL` 2) :: Word)
   test ((42 `unsafeShiftR` 2) :: Int)
   test ((42 `unsafeShiftL` 2) :: Int)

test :: Show a => a -> IO ()
test a = print a `catch` (\Overflow -> putStrLn "Overflow!")
