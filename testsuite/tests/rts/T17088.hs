{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

import Data.Word
import Foreign.Storable
import GHC.Prim
import GHC.Ptr
import GHC.Types
import System.IO.Unsafe

----------------------------------------------------------------

allocAndFreeze :: Int -> (Ptr p -> IO ()) -> Bytes
allocAndFreeze sz f = unsafePerformIO (bytesAllocRet sz f)
{-# NOINLINE allocAndFreeze #-}

data Bytes = Bytes (MutableByteArray# RealWorld)
data IBA   = IBA   (ByteArray#)

instance Show Bytes where
    showsPrec p b = showsPrec p (bytesUnpackChars b)

------------------------------------------------------------------------

bytesAllocRet :: Int -> (Ptr p -> IO ()) -> IO Bytes
bytesAllocRet (I# sz) f = do
  !arr@(Bytes mba) <- IO $ \s -> case newAlignedPinnedByteArray# sz 8# s of
                                   (# s', mba #) -> (# s', Bytes mba #)
  !(IBA ba)        <- IO $ \s -> case unsafeFreezeByteArray# mba s of
                                   (# s' , ba #) -> (# s' , IBA ba #)
  f (Ptr (byteArrayContents# ba))
  IO $ \s -> case touch# mba s of
               s' -> (# s', () #)
  return arr

------------------------------------------------------------------------

bytesEq :: Bytes -> Bytes -> Bool
bytesEq (Bytes m1) (Bytes m2)
    | tagToEnum# (len /=# len') = False
    | otherwise                 = unsafePerformIO $ IO $ \s -> loop 0# s
  where
    !len  = sizeofMutableByteArray# m1
    !len' = sizeofMutableByteArray# m2

    loop i s
      | tagToEnum# (i ==# len) = (# s, True #)
      | otherwise              =
          case readWord8Array# m1 i s of
            (# s', e1 #) ->
              case readWord8Array# m2 i s' of
                (# s'', e2 #) ->
                  if tagToEnum# (eqWord# e1 e2)
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
  $ map (go . fromIntegral) [1::Int, 2.. ]
  where
    go a = allocAndFreeze 1 $ \p -> poke p (a :: Word8)

main :: IO ()
main = do
  putStrLn $ "-- 1"
  -- NOTE: Bang is important here
  let !pubK = head publicKeyStream
  putStrLn $ "-- 2"
  let (!k1) : (!k2) : _ = [ pk
                          | pk <- reverse publicKeyStream
                          , bytesEq pk pubK
                          ]
  print (k1,k2)
  putStrLn "OK"
