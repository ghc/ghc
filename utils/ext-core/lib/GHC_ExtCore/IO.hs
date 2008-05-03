{-# OPTIONS -fglasgow-exts #-}

-- Replacement for GHC.IO module

module GHC_ExtCore.IO where

import GHC.Exts
import GHC_ExtCore.Handle

hPutStr :: Handle -> String -> IO ()
hPutStr h s = mapM_ (hPutChar h) s

hPutChar :: Handle -> Char -> IO ()
hPutChar (H (I# i)) (C# c) = hPutChar# i c

------------------------------------------------------------
-- fake stubs for primops to fool GHC into typechecking this
------------------------------------------------------------
{-# NOINLINE hPutChar# #-}
hPutChar# :: Int# -> Char# -> IO ()
hPutChar# _ _ = return ()
