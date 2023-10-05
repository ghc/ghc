{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}

-- | This module is the driver for the Cmm machop tests. It expects to be
-- linked with an object file (typically compiled Cmm) exposing a procedure
-- named `test` which'
--
--   - takes a single pointer argument pointing to a buffer containing
--     [0..bufferSz] (truncated to Word8).
--
--   - returns a Word#
--
-- The driver will print the returned result.

module Main where

import GHC.Exts
import GHC.Ptr
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

foreign import prim "test" test :: Addr# -> Word#

bufferSz :: Int
bufferSz = 1*1024*1024

main :: IO ()
main = do
  let buf = BS.pack $ map fromIntegral [0..bufferSz]
  BS.unsafeUseAsCString buf $ \(Ptr p) -> do
    print $ W# (test p)

