{-# LANGUAGE ForeignFunctionInterface #-}
import Foreign
import Foreign.C

-- test for calling functions by importing them by address and then
-- using dynamic calls.  In 6.10 and earlier, GHCi rejected the
-- foreign import '&foo' declarations, for no apparently good reason.

type Malloc = CSize -> IO (Ptr ())
type Write = CInt -> Ptr CChar -> CSize -> IO CSize

foreign import ccall unsafe "&malloc" pmalloc:: FunPtr Malloc
foreign import ccall unsafe "dynamic" callMalloc :: FunPtr Malloc -> Malloc

foreign import ccall unsafe "&write" pwrite:: FunPtr Write
foreign import ccall unsafe "dynamic" callWrite :: FunPtr Write -> Write

main = do
  p <- callMalloc pmalloc 32
  free p
  withCStringLen "hello\n" $ \(p,len) -> callWrite pwrite 1 p (fromIntegral len)
  return ()
