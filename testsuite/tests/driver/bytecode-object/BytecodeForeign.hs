{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module BytecodeForeign where

import Foreign.C
import Language.Haskell.TH.Syntax

-- Simple foreign function that adds two integers
foreign import ccall "add_ints" addInts :: CInt -> CInt -> CInt

-- Foreign function that returns a constant
foreign import ccall "get_constant" getConstant :: IO CInt

-- Test function that uses the foreign functions
testForeign :: IO ()
testForeign = do
    let result1 = addInts 5 3
    result2 <- getConstant
    print (fromIntegral result1 :: Int)
    print (fromIntegral result2 :: Int)

-- Pure function that can be called from other modules
pureFunction :: Int -> Int -> Int
pureFunction x y = x + y

$(addForeignFilePath LangC "BytecodeForeign.c" >> return [])

