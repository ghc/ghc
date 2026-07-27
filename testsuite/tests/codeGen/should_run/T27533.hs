{-# LANGUAGE MagicHash, UnboxedTuples, GHCForeignImportPrim, UnliftedFFITypes #-}

import GHC.Exts
import GHC.IO (IO(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr(..))
import Numeric (showHex)
import System.IO

foreign import prim "store8" store8# :: Addr# -> Word#
foreign import prim "load8"  load8#  :: Addr# -> Word#

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  allocaBytes 8 $ \(Ptr a) -> do
    -- 1. Silent corruption: release-store of 1 byte into an all-ones word.
    IO (\s -> (# writeWordOffAddr# a 0# 0xFFFFFFFFFFFFFFFF## s, () #))
    case store8# a of _ -> return ()   -- case on unlifted Word# forces the call
    w <- IO (\s -> case readWordOffAddr# a 0# s of (# s', v #) -> (# s', W# v #))
    putStrLn ("after 1-byte release-store: 0x" ++ showHex w "")
    -- expected 0xffffffffffffff43; buggy NCG gives 0xffffffff00000043

    -- 2. SIGBUS: acquire-load of 1 byte at an odd address (well-defined).
    r <- IO (\s -> (# s, W# (load8# (a `plusAddr#` 1#)) #))
    putStrLn ("acquire byte load at p+1:   0x" ++ showHex r "")
    -- expected 0xff; buggy NCG dies with SIGBUS before printing
