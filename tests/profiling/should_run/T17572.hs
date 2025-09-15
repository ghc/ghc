{-# LANGUAGE UnboxedTuples, MagicHash, BlockArguments #-}

import GHC.Exts
import GHC.Types

doSomething :: Word -> IO Word
doSomething (W# x) = IO \s ->
   case newByteArray# 7096# s of -- we need a large ByteArray#
      (# s, mba #) -> case shrinkMutableByteArray# mba 7020# s of -- shrunk
            s -> case unsafeFreezeByteArray# mba s of
               (# s, ba #) -> (# s, W# (indexWordArray# ba 18#) #)

main :: IO ()
main = do
   xs <- mapM doSomething [0..300000] -- we need enough elements (to trigger a GC maybe?)
   print (length xs)
