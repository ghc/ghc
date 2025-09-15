{-# LANGUAGE ForeignFunctionInterface #-}
module T11983 where



import Foreign.Ptr

foreign import ccall "intptr_example"
    intPtrExample :: IntPtr -> IO ()
foreign import ccall "uintptr_example"
    wordPtrExample :: WordPtr -> IO ()
