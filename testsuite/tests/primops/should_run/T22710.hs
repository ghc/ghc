-- | Test 'unsafeThawByteArray#'.

{-# LANGUAGE MagicHash, UnboxedTuples #-}

import GHC.Exts (newByteArray#, indexWord8Array#, writeWord8Array#,
                 unsafeFreezeByteArray#, unsafeThawByteArray#,
                 ByteArray#, MutableByteArray#, Int(I#))
import GHC.Word
import GHC.ST
import Prelude hiding (toList)

main :: IO ()
main = do
    res <- return $ runST $ do
        let n = 32 :: Int
        marr <- newByteArray n
        mapM_ (\i -> writeWord8Array marr i (fromIntegral i)) [0..n-1]
        arr <- unsafeFreezeByteArray marr
        marr' <- unsafeThawByteArray arr
        arr' <- unsafeFreezeByteArray marr'
        return $ toList arr' 5

    print res

data ByteArray = ByteArray { unBA :: ByteArray# }
data MByteArray s = MByteArray { unMBA :: MutableByteArray# s }

newByteArray :: Int -> ST s (MByteArray s)
newByteArray (I# n#) = ST $ \s# -> case newByteArray# n# s# of
    (# s2#, marr# #) -> (# s2#, MByteArray marr# #)

indexWord8Array :: ByteArray -> Int -> Word8
indexWord8Array arr (I# i#) = case indexWord8Array# (unBA arr) i# of
    a -> W8# a

writeWord8Array :: MByteArray s -> Int -> Word8 -> ST s ()
writeWord8Array marr (I# i#) (W8# a) = ST $ \ s# ->
    case writeWord8Array# (unMBA marr) i# a s# of
        s2# -> (# s2#, () #)

unsafeFreezeByteArray :: MByteArray s -> ST s ByteArray
unsafeFreezeByteArray marr = ST $ \ s# ->
    case unsafeFreezeByteArray# (unMBA marr) s# of
        (# s2#, arr# #) -> (# s2#, ByteArray arr# #)

unsafeThawByteArray :: ByteArray -> ST s (MByteArray s)
unsafeThawByteArray arr = ST $ \ s# ->
    case unsafeThawByteArray# (unBA arr) s# of
        (# s2#, marr# #) -> (# s2#, MByteArray marr# #)

toList :: ByteArray -> Int -> [Word8]
toList arr n = go 0
  where
    go i | i >= n = []
         | otherwise = indexWord8Array arr i : go (i+1)
