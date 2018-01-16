{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString.Char8 as S
import Data.ByteString.Internal
import Data.ByteString.Unsafe
import Foreign
import Control.Monad
import System.Environment
import System.CPUTime
import Text.Printf
import Foreign.ForeignPtr

time :: Show t => IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    v `seq` return ()
    end   <- getCPUTime
    print v
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

main = do
    [f,g,n,m] <- getArgs
    x <- S.readFile f
    y <- S.readFile g
    forM_ [read n .. read m] $ \i -> do
        print i
        time (-- replicateM_ 100000000 $
                    return $ S.take i x `compareBytes`  S.take i y
                            )
        time (-- replicateM_ 100000000 $
                    return $ S.take i x `compareBytesC` S.take i y
                            )

------------------------------------------------------------------------

compareBytes :: ByteString -> ByteString -> Ordering
compareBytes (PS fp1 off1 len1) (PS fp2 off2 len2)
--    | len1 == 0  && len2 == 0                     = EQ  -- short cut for empty strings
--    | fp1 == fp2 && off1 == off2 && len1 == len2  = EQ  -- short cut for the same string
    | otherwise                                   = inlinePerformIO $
    withForeignPtr fp1 $ \p1 ->
        withForeignPtr fp2 $ \p2 ->
            cmp (p1 `plusPtr` off1)
                (p2 `plusPtr` off2) 0 len1 len2
 
cmp :: Ptr Word8 -> Ptr Word8 -> Int -> Int -> Int-> IO Ordering
cmp p1 p2 n len1 len2
      | n == len1 = if n == len2 then return EQ else return LT
      | n == len2 = return GT
      | otherwise = do
          a <- peekByteOff p1 n :: IO Word8
          b <- peekByteOff p2 n
          case a `compare` b of
                EQ -> cmp p1 p2 (n+1) len1 len2
                LT -> return LT
                GT -> return GT
 
compareBytesC (PS x1 s1 l1) (PS x2 s2 l2)
    | l1 == 0  && l2 == 0               = EQ  -- short cut for empty strings
    | x1 == x2 && s1 == s2 && l1 == l2  = EQ  -- short cut for the same string
    | otherwise                         = inlinePerformIO $
        withForeignPtr x1 $ \p1 ->
        withForeignPtr x2 $ \p2 -> do
            i <- memcmp (p1 `plusPtr` s1) (p2 `plusPtr` s2) (fromIntegral $ min l1 l2)
            return $! case i `compare` 0 of
                        EQ  -> l1 `compare` l2
                        x   -> x
{-# INLINE compareBytes #-}

