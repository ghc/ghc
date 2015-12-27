{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import Control.Monad (unless)
import GHC.Exts
import GHC.Types

data ByteArray s = BA (MutableByteArray# s)

main :: IO ()
main = do
    ba# <- IO (\s0 -> case newByteArray# 256# s0 of
                        (# s1, ba# #) -> (# s1, BA ba# #))
    let go n = do
            shrink ba# n
            sz <- getSize ba#
            unless (sz == n) $ print (sz, n)
    mapM go [128, 64, 63, 32, 2, 1]
    return ()

shrink :: ByteArray RealWorld -> Int -> IO ()
shrink (BA ba#) (I# n#) = IO (\s ->
    case shrinkMutableByteArray# ba# n# s of
      s' -> (# s', () #))

getSize :: ByteArray RealWorld -> IO Int
getSize (BA ba#) = IO (\s ->
    case getSizeofMutableByteArray# ba# s of
      (# s', n# #) -> (# s', I# n# #))

