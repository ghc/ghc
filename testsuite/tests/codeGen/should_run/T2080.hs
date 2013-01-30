{-# OPTIONS_GHC -XMagicHash #-}
module Main where
import GHC.Base

import Foreign
import Foreign.C
import GHC.Ptr (Ptr(..))

utf8DecodeChar# :: Addr# -> Bool -> Bool
{-# NOINLINE utf8DecodeChar# #-}
utf8DecodeChar# a# fred =
  case () of 
    _ | word2Int# (indexWord8OffAddr# a# 0#) <=# 0x7F# -> True

-- Omitting the next line gives an ASSERT error:
-- ghc-6.9: panic! (the 'impossible' happened)
--   (GHC version 6.9 for x86_64-unknown-linux):
-- 	ASSERT failed! file nativeGen/MachCodeGen.hs line 1049
-- %MO_S_Le_I8(I8[R2], 127 :: I8)
      | fred -> True

      | otherwise -> False

main = print (utf8DecodeChar# "\128"# False) -- should be False

