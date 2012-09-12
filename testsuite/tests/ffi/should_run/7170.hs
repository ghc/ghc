{-# LANGUAGE ForeignFunctionInterface #-}
import Control.Concurrent
import Control.Exception    (bracket)
import Foreign.Ptr          (Ptr, intPtrToPtr, ptrToIntPtr)
import Foreign.ForeignPtr   (ForeignPtr)
import qualified Foreign.Concurrent as FC
import qualified Foreign.ForeignPtr as FP

testForeignPtr_Concurrent :: Ptr a -> IO (ForeignPtr a)
testForeignPtr_Concurrent ptr = FC.newForeignPtr ptr (fin ptr)

fin :: Ptr a -> IO ()
fin ptr = putStrLn $ "finalizing " ++ show (fromIntegral (ptrToIntPtr ptr) :: Int)

main :: IO ()
main = do
    mv <- newEmptyMVar
    bracket (testForeignPtr_Concurrent $ intPtrToPtr 1)
            FP.finalizeForeignPtr $ \_ ->
        -- hang, so the thread and foreign pointer get GCed
        takeMVar mv
