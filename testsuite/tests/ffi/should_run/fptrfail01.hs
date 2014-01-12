{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign
import GHC.ForeignPtr

{-# INCLUDE "fptrfail01.h" #-}

foreign import ccall "&f" fptr :: FunPtr (Ptr Int -> IO ())

main :: IO ()
main = do
    with (33 :: Int) test
    where
        test p = do
            f <- newForeignPtr_ p
            addForeignPtrFinalizer fptr f
            addForeignPtrConcFinalizer f (putStrLn "Haskell finalizer")
