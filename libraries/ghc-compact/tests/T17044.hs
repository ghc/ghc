{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

import Data.Traversable (for)
import GHC.Compact
import GHC.Exts
import GHC.IO

main :: IO ()
main = do
  c <- compact ()
  big <- newByteArray 1032128
  bigFrozen <- unsafeFreezeByteArray big
  c' <- compactAdd c bigFrozen

  _placeholders <- for [0 :: Int .. 2044] $ \i -> do
    getCompact <$> compactAdd c' i

  return ()

data ByteArray = ByteArray ByteArray#

data MutableByteArray s = MutableByteArray (MutableByteArray# s)

newByteArray :: Int -> IO (MutableByteArray RealWorld)
newByteArray (I# n#) = IO (\s# -> case newByteArray# n# s# of (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #))

unsafeFreezeByteArray :: MutableByteArray RealWorld -> IO ByteArray
unsafeFreezeByteArray (MutableByteArray arr#) = IO (\s# -> case unsafeFreezeByteArray# arr# s# of (# s'#, arr'# #) -> (# s'#, ByteArray arr'# #))
