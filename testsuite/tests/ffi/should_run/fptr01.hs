{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign

{-# INCLUDE "fptr01.h" #-}

foreign import ccall "&f" fptr :: FunPtr (Ptr Int -> IO ())
foreign import ccall "&g" gptr :: FunPtr (Ptr Int -> IO ())
foreign import ccall "&h" hptr :: FunPtr (Ptr Int -> IO ())

foreign import ccall "&f_env" fenvptr :: FunPtr (Ptr Int -> Ptr Int -> IO ())

main :: IO ()
main = do
    with (33 :: Int) ((>>= finalizeForeignPtr) . test)
    with (34 :: Int) ((>>  return ())          . test)
    with (35 :: Int) ((>>= finalizeForeignPtr) . test_env)
    with (36 :: Int) ((>>  return ())          . test_env)
    -- finalizers must all be run at program exit.
    where
        -- the finalizers must be run in the correct order, starting with
        -- the most recently-added.
        test p = do
            f <- newForeignPtr_ p
            addForeignPtrFinalizer fptr f
            addForeignPtrFinalizer gptr f
            addForeignPtrFinalizer hptr f
            return f

        test_env p = do
            f <- newForeignPtr_ p
            envp1 <- new 1
            envp2 <- new 2
            envp3 <- new 3
            addForeignPtrFinalizerEnv fenvptr envp1 f
            addForeignPtrFinalizerEnv fenvptr envp2 f
            addForeignPtrFinalizerEnv fenvptr envp3 f
            return f
