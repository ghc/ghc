{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CApiFFI #-}

module Main where

import Data.List (foldl')
import Data.Bits
import GHC.Ptr
import Foreign.Ptr
import Foreign.C.Types

type FD = CInt
type CString = Ptr CChar

foreign import ccall unsafe "runInteractiveProcess"
  c_runInteractiveProcess
        :: Ptr CString
        -> CString
        -> Ptr CString
        -> FD
        -> FD
        -> FD
        -> Ptr FD
        -> Ptr FD
        -> Ptr FD
        -> Ptr CInt
        -> Ptr CInt
        -> CInt                         -- flags
        -> Ptr CString
        -> IO CInt

main = do
  c_runInteractiveProcess
    (f 0x1)
    (f 0x2)
    (f 0x3)

    0x4 0x5 0x6

    (f 0x7)
    (f 0x8)
    (f 0x9)

    (f 0xa)
    (f 0xb)

    0xcccccccc

    (f 0xd)

-- | Fill a word with a nibble.
-- e.g. @f 1 == 0x1111111111111111@.
f :: Int -> Ptr a
f n = intPtrToPtr $ fromIntegral $ foldl' (.|.) 0 (map (n `shiftL`) [0,4..60])

