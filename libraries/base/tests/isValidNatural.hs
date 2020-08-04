{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import GHC.Num.Natural
import GHC.Num.BigNat
import GHC.Exts
import GHC.IO

main = do
   let
      maxWord = fromIntegral (maxBound :: Word)
      invalid = NB (bigNatOne# (# #)) -- 1 would fit into the NS constructor.

   -- byteArray whose size is not a multiple of Word size
   invalid2 <- IO $ \s -> case newByteArray# 27# s of
                           (# s', mba #) -> case unsafeFreezeByteArray# mba s' of
                              (# s'', ba #) -> (# s'', NB ba #)

   print $ map naturalCheck [0, 1, maxWord, maxWord + 1, invalid, invalid2]
