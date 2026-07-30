{-# LANGUAGE MagicHash, UnboxedTuples, GHCForeignImportPrim, UnliftedFFITypes #-}

import GHC.Exts
import GHC.IO (IO(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr(..))
import Data.Word (Word8)
import Numeric (showHex)
import System.IO

foreign import prim "store8" store8# :: Addr# -> Word#
foreign import prim "load8"  load8#  :: Addr# -> Word#

-- Read one byte at a given byte offset. Working a byte at a time keeps the
-- test independent of both endianness and word size: the buffer contents are
-- a fixed sequence of bytes in address order, whereas a word-sized read of
-- the same buffer would give 0x..43 on little-endian and 0x43.. on big-endian.
readByte :: Addr# -> Int -> IO Word
readByte a (I# i) =
  IO (\s -> case readWord8OffAddr# a i s of
              (# s', v #) -> (# s', W# (word8ToWord# v) #))

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  allocaBytes 8 $ \p@(Ptr a) -> do
    -- 1. Silent corruption: release-store of 1 byte into an all-ones buffer.
    --    The store must touch byte 0 and leave bytes 1..7 alone; a buggy NCG
    --    widens it to a 4-byte store and zeroes bytes 1..3.
    fillBytes p (0xFF :: Word8) 8
    case store8# a of _ -> return ()   -- case on unlifted Word# forces the call
    bs <- mapM (readByte a) [0 .. 7]
    putStrLn ("after 1-byte release-store: " ++ unwords (map (\b -> showHex b "") bs))
    -- expected  43 ff ff ff ff ff ff ff
    -- buggy NCG gives  43 0 0 0 ff ff ff ff

    -- 2. SIGBUS: acquire-load of 1 byte at an odd address (well-defined).
    r <- IO (\s -> (# s, W# (load8# (a `plusAddr#` 1#)) #))
    putStrLn ("acquire byte load at p+1:   0x" ++ showHex r "")
    -- expected 0xff; buggy NCG dies with SIGBUS before printing
