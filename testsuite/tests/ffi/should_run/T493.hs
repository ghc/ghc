import Foreign
import Foreign.C

-- These newtypes...
newtype MyFunPtr a = MyFunPtr { getFunPtr :: FunPtr a }
newtype MyPtr a = MyPtr (Ptr a)
newtype MyIO a = MyIO { runIO :: IO a }
-- should be supported by...

-- foreign import dynamics
foreign import ccall "dynamic"
    mkFun1 :: MyFunPtr (CInt -> CInt) -> (CInt -> CInt)
foreign import ccall "dynamic"
    mkFun2 :: MyPtr (Int32 -> Int32) -> (CInt -> CInt)

-- and foreign import wrappers.
foreign import ccall "wrapper"
    mkWrap1 :: (CInt -> CInt) -> MyIO (MyFunPtr (CInt -> CInt))
foreign import ccall "wrapper"
    mkWrap2 :: (CInt -> CInt) -> MyIO (MyPtr (Int32 -> Int32))

-- We'll need a dynamic function point to export
foreign import ccall "getDbl" getDbl :: IO (MyFunPtr (CInt -> CInt))
-- and a Haskell function to export
half :: CInt -> CInt
half = (`div` 2)
-- and a C function to pass it to.
foreign import ccall "apply" apply1 :: MyFunPtr (CInt -> CInt) -> Int -> Int
foreign import ccall "apply" apply2 :: MyPtr (Int32 -> Int32) -> Int -> Int

main :: IO ()
main = do

  dbl <- getDbl
  let dbl1 = mkFun1 dbl
      dbl2 = mkFun2 $ MyPtr $ castFunPtrToPtr $ getFunPtr dbl
  print (dbl1 21, dbl2 21)

  half1 <- runIO $ mkWrap1 half
  half2 <- runIO $ mkWrap2 half
  print (apply1 half1 84, apply2 half2 84)
