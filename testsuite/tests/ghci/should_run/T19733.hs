{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# LANGUAGE UnliftedFFITypes    #-}

{-# OPTIONS_GHC -fbyte-code      #-}

module Main where

import           GHC.Exts
import           GHC.IO
import           Foreign.C

main :: IO ()
main = testThreadId >> putStrLn "OK"

testThreadId :: IO CInt
testThreadId = IO $ \s0 ->
    case myThreadId# s0 of
        (# s1, tid #) -> (# s1, c_getThreadId tid #)

foreign import ccall unsafe "rts_getThreadId" c_getThreadId :: ThreadId# -> CInt

