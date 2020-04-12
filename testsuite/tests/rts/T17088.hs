{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module Main (main) where

import Data.Word
import Foreign.Storable
import GHC.Prim
import GHC.Ptr
import GHC.Types
import System.IO.Unsafe

----------------------------------------------------------------

allocAndFreeze :: Int -> Bytes
allocAndFreeze sz = unsafePerformIO (bytesAllocRet sz)

data Bytes = Bytes (MutableByteArray# RealWorld)
data IBA   = IBA   (ByteArray#)

instance Show Bytes where
    showsPrec p b = showsPrec p (bytesUnpackChars b)

------------------------------------------------------------------------

bytesAllocRet :: Int -> IO Bytes
bytesAllocRet (I# sz) =
  IO $ \s -> case newAlignedPinnedByteArray# sz 8# s of
               (# s', mba #) -> (# s', Bytes mba #)

------------------------------------------------------------------------

bytesEq :: Bytes -> Bytes -> Bool
bytesEq (Bytes m1) (Bytes m2)
    | isTrue# (len /=# len') = False
    | otherwise              = unsafePerformIO $ IO $ \s -> loop 0# s
  where
    !len  = sizeofMutableByteArray# m1
    !len' = sizeofMutableByteArray# m2

    loop i s
      | isTrue# (i ==# len) = (# s, True #)
      | otherwise              =
          case readWord8Array# m1 i s of
            (# s', e1 #) ->
              case readWord8Array# m2 i s' of
                (# s'', e2 #) ->
                  if isTrue# (eqWord# e1 e2)
                    then loop (i +# 1#) s''
                    else (# s'', False #)


bytesUnpackChars :: Bytes -> String
bytesUnpackChars (Bytes mba)
  | I# (sizeofMutableByteArray# mba) == 0 = []
  | otherwise = unsafePerformIO $ do
      c <- IO $ \s -> case readWord8Array# mba 0# s of
                        (# s'', w #) -> (# s'', C# (chr# (word2Int# w)) #)
      return [c]

----------------------------------------------------------------

publicKeyStream :: [Bytes]
publicKeyStream
  = take 10000
  $ map (go . fromIntegral) [1::Int ..]
  where
    go :: Word8 -> Bytes
    go a = allocAndFreeze 1

main :: IO ()
main = do
  let !pubK = head publicKeyStream
  let (!k1) : _ = [ pk
                  | pk <- reverse publicKeyStream
                  , bytesEq pk pubK
                  ]
  print k1
