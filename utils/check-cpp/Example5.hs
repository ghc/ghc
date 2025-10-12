{-# LANGUAGE GHC_CPP #-}

module Example5 where

sendMessage fd msg = do
  fromIntegral

#if defined(HAVE_EVENTFD)
foreign import ccall unsafe "sys/eventfd.h eventfd_write"
   c_eventfd_write :: CInt -> CULLong -> IO CInt
#endif

foreign import ccall unsafe "setIOManagerWakeupFd"
   c_setIOManagerWakeupFd :: CInt -> IO ()
