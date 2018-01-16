module Main where

import Foreign
import Foreign.C
import Text.Printf
import System.Exit
import Control.Monad

foreign import ccall "getesp" getesp :: IO CInt

main = do
  checkSpAlignment
  wrap checkSpAlignment >>= run
  wrap1 args1 >>= \f -> run1 f 3
  wrap2 args2 >>= \f -> run2 f 3 4
  wrap3 args3 >>= \f -> run3 f 3 4 5
  wrap4 args4 >>= \f -> run4 f 3 4 5 6

foreign import ccall "wrapper" wrap :: IO () -> IO (FunPtr (IO ()))
foreign import ccall "dynamic" run  :: FunPtr (IO ()) -> IO ()

type Args1 = Int -> IO ()

foreign import ccall "wrapper" wrap1 :: Args1 -> IO (FunPtr Args1)
foreign import ccall "dynamic" run1  :: FunPtr Args1 -> Args1

args1 :: Args1
args1 _ = checkSpAlignment

type Args2 = Int -> Int -> IO ()

foreign import ccall "wrapper" wrap2 :: Args2 -> IO (FunPtr Args2)
foreign import ccall "dynamic" run2  :: FunPtr Args2 -> Args2

args2 :: Args2
args2 _ _ = checkSpAlignment

type Args3 = Int -> Int -> Int -> IO ()

foreign import ccall "wrapper" wrap3 :: Args3 -> IO (FunPtr Args3)
foreign import ccall "dynamic" run3  :: FunPtr Args3 -> Args3

args3 :: Args3
args3 _ _ _ = checkSpAlignment

type Args4 = Int -> Int -> Int -> Int -> IO ()

foreign import ccall "wrapper" wrap4 :: Args4 -> IO (FunPtr Args4)
foreign import ccall "dynamic" run4  :: FunPtr Args4 -> Args4

args4 :: Args4
args4 _ _ _ _ = checkSpAlignment

checkSpAlignment :: IO ()
checkSpAlignment = do
  esp <- getesp
  when (((esp + fromIntegral (sizeOf (undefined :: Ptr ()))) .&. 15) /= 0) $ do
     printf "esp not aligned correctly: %x\n" (fromIntegral esp :: Word32)
     exitWith (ExitFailure 1)

