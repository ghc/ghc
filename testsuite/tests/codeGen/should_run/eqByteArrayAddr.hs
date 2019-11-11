{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE UnboxedTuples #-}

module Main (main) where

import Data.Char (ord)
import GHC.Exts
import GHC.ST
import GHC.Word

main :: IO ()
main = do
    putStrLn "BEGIN"
    print (eqByteArrayAddr s1 0 "barg"# 4)
    print (eqByteArrayAddr s2 0 "bash"# 4)
    print (eqByteArrayAddr s1 0 "barn"# 4)
    print (eqByteArrayAddr s1 0 "bargle"# 4)

data BA    = BA# ByteArray#
data MBA s = MBA# (MutableByteArray# s)

eqByteArrayAddr :: BA -> Int -> Addr# -> Int -> Bool
eqByteArrayAddr (BA# ba1#) (I# ofs1#) addr2# (I# n#)
  = isTrue# (eqByteArrayAddr# ba1# ofs1# addr2# n#)

s1, s2 :: BA
s1 = pack [c2w 'b', c2w 'a', c2w 'r', c2w 'g']
s2 = pack [c2w 'b', c2w 'a', c2w 's', c2w 'h']

c2w :: Char -> Word8
c2w = fromIntegral . ord

pack :: [Word8] -> BA
pack xs = createByteArray (length xs) $ \mba -> do
    let  go _ [] = pure ()
         go i (y:ys) = do
           writeWord8Array mba i y
           go (i+1) ys
    go 0 xs

newByteArray :: Int -> ST s (MBA s)
newByteArray (I# n#)
  = ST $ \s -> case newByteArray# n# s of
                 (# s', mba# #) -> (# s', MBA# mba# #)

writeWord8Array :: MBA s -> Int -> Word8 -> ST s ()
writeWord8Array (MBA# mba#) (I# i#) (W8# j#)
  = ST $ \s -> case writeWord8Array# mba# i# j# s of
                 s' -> (# s', () #)

createByteArray :: Int -> (forall s. MBA s -> ST s ()) -> BA
createByteArray n go = runST $ do
    mba <- newByteArray n
    go mba
    unsafeFreezeByteArray mba

unsafeFreezeByteArray :: MBA s -> ST s BA
unsafeFreezeByteArray (MBA# mba#)
  = ST $ \s -> case unsafeFreezeByteArray# mba# s of
                 (# s', ba# #) -> (# s', BA# ba# #)

