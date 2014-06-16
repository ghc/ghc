{-# options -fffi #-}

import Foreign
import Foreign.C.Types

main = do
    wprint <- wrap8 print
    call8 wprint
    wprint <- wrap16 print
    call16 wprint
    wprint <- wrap32 print
    call32 wprint
    wprint <- wrap64 print
    call64 wprint

foreign import ccall "wrapper"
    wrap8 :: (Int8 -> IO ()) -> IO (FunPtr (Int8 -> IO ()))

foreign import ccall "FunPtrBug.h call8"
    call8 :: FunPtr (Int8 -> IO ()) -> IO ()

foreign import ccall "wrapper"
    wrap16 :: (Int16 -> IO ()) -> IO (FunPtr (Int16 -> IO ()))

foreign import ccall "FunPtrBug.h call16"
    call16 :: FunPtr (Int16 -> IO ()) -> IO ()

foreign import ccall "wrapper"
    wrap32 :: (Int32 -> IO ()) -> IO (FunPtr (Int32 -> IO ()))

foreign import ccall "FunPtrBug.h call32"
    call32 :: FunPtr (Int32 -> IO ()) -> IO ()

foreign import ccall "wrapper"
    wrap64 :: (Int64 -> IO ()) -> IO (FunPtr (Int64 -> IO ()))

foreign import ccall "FunPtrBug.h call64"
    call64 :: FunPtr (Int64 -> IO ()) -> IO ()
