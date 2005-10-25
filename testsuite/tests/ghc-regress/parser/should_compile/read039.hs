{-# LANGUAGE ForeignFunctionInterface, CPP #-}
-- Test the LANGUAGE pragma
module ShouldCompile where

#if 1
foreign import ccall "foo" foo :: Int -> IO Int
#endif
