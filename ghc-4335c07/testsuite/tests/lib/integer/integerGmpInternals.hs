{-# LANGUAGE BangPatterns, CPP, MagicHash, UnboxedTuples #-}

module Main (main) where

import Data.List (group)
import Data.Bits
import Data.Word
import Control.Monad

import GHC.Word
import GHC.Base
import GHC.Integer.GMP.Internals (Integer(S#,Jp#,Jn#))
import qualified GHC.Integer.GMP.Internals as I

-- NOTE: Some of the following operations were provided with
-- integer-gmp-0.5.1, but were not ported to integer-gmp-1.0.0 (yet);
-- so we use naive reference-implementations instead for the meantime
-- in order to keep the reference-output untouched.

recipModInteger :: Integer -> Integer -> Integer
recipModInteger = I.recipModInteger

gcdExtInteger :: Integer -> Integer -> (Integer, Integer)
gcdExtInteger a b = case I.gcdExtInteger a b of (# g, s #) -> (g, s)

powModSecInteger :: Integer -> Integer -> Integer -> Integer
powModSecInteger = I.powModSecInteger

powModInteger :: Integer -> Integer -> Integer -> Integer
powModInteger = I.powModInteger

-- FIXME: Lacks GMP2 version
powInteger :: Integer -> Word -> Integer
powInteger x e = x^e

exportInteger :: Integer -> MutableByteArray# RealWorld -> Word# -> Int# -> IO Word
exportInteger = I.exportIntegerToMutableByteArray

exportIntegerAddr :: Integer -> Addr# -> Int# -> IO Word
exportIntegerAddr = I.exportIntegerToAddr

importInteger :: ByteArray# -> Word# -> Word# -> Int# -> Integer
importInteger = I.importIntegerFromByteArray

importIntegerAddr :: Addr# -> Word# -> Int# -> IO Integer
importIntegerAddr a l e = I.importIntegerFromAddr a l e

-- helpers
data MBA = MBA { unMBA :: !(MutableByteArray# RealWorld) }
data BA  = BA  { unBA  :: !ByteArray# }

newByteArray :: Word# -> IO MBA
newByteArray sz = IO $ \s -> case newPinnedByteArray# (word2Int# sz) s of (# s, arr #) -> (# s, MBA arr #)

indexByteArray :: ByteArray# -> Word# -> Word8
indexByteArray a# n# = W8# (indexWord8Array# a# (word2Int# n#))

-- indexMutableByteArray :: MutableByteArray# RealWorld -> Word# -> IO Word8
-- indexMutableByteArray a# n# = IO $ \s -> case readWord8Array# a# (word2Int# n#) s of (# s', v #) -> (# s', W# v #)

writeByteArray :: MutableByteArray# RealWorld -> Int# -> Word8 -> IO ()
writeByteArray arr i (W8# w) = IO $ \s -> case writeWord8Array# arr i w s of s -> (# s, () #)

lengthByteArray :: ByteArray# -> Word
lengthByteArray ba = W# (int2Word# (sizeofByteArray# ba))

unpackByteArray :: ByteArray# -> [Word8]
unpackByteArray ba | n == 0    = []
                   | otherwise = [ indexByteArray ba i | W# i <- [0 .. n-1] ]
  where
    n = lengthByteArray ba

freezeByteArray :: MutableByteArray# RealWorld -> IO BA
freezeByteArray arr = IO $ \s -> case unsafeFreezeByteArray# arr s of (# s, arr #) -> (# s, BA arr #)

----------------------------------------------------------------------------
main :: IO ()
main = do
    print $ powModInteger b e m
    print $ powModInteger b e (m-1)
    print $ powModSecInteger b e (m-1)
    print $ gcdExtInteger b e
    print $ gcdExtInteger e b
    print $ gcdExtInteger x y
    print $ gcdExtInteger y x
    print $ gcdExtInteger x (-y)
    print $ gcdExtInteger (-x) y
    print $ gcdExtInteger (-x) (-y)
    print $ powInteger 12345 0
    print $ powInteger 12345 1
    print $ powInteger 12345 30
    print $ [ (x,i) | x <- [-7..71], let i = recipModInteger x (2*3*11*11*17*17), i /= 0 ]
    print $ I.nextPrimeInteger b
    print $ I.nextPrimeInteger e
    print $ [ k | k <- [ 0 .. 200 ], S# (I.testPrimeInteger k 25#) `elem` [1,2] ]
    print $ rle [ S# (I.testPrimeInteger k 25#) | k <- [ x .. x + 1000 ] ]
    print $ rle [ S# (I.testPrimeInteger k 25#) | k <- [ e .. e + 1000 ] ]

    -- import/export primitives
    print $ [ W# (I.sizeInBaseInteger x 2#)   | x <- [b1024,b*e,b,e,m,x,y,-1,0,1] ]
    print $ [ W# (I.sizeInBaseInteger x 256#) | x <- [b1024,b*e,b,e,m,x,y,-1,0,1] ]

    BA ba <- do
        MBA mba <- newByteArray 128##
        forM_ (zip [0..127] [0x01..]) $ \(I# i, w) -> do
            writeByteArray mba i w

        let a = byteArrayContents# (unsafeCoerce# mba)

        print =<< importIntegerAddr a 0## 1#
        print =<< importIntegerAddr a 0## 0#

        print =<< importIntegerAddr (plusAddr# a 22#) 1## 1#
        print =<< importIntegerAddr (plusAddr# a 97#) 1## 0#

        print =<< importIntegerAddr a 23## 1#
        print =<< importIntegerAddr a 23## 0#

        -- no-op
        print =<< exportIntegerAddr 0 (plusAddr# a 0#) 1#

        -- write into array
        print =<< exportIntegerAddr b (plusAddr# a  5#) 1#
        print =<< exportIntegerAddr e (plusAddr# a 50#) 0#

        print =<< exportInteger m mba 85## 1#
        print =<< exportInteger m mba 105## 0#

        print =<< importIntegerAddr (plusAddr# a 85#)  17## 1#
        print =<< importIntegerAddr (plusAddr# a 105#) 17## 0#

        -- read back full array
        print =<< importIntegerAddr a 128## 1#
        print =<< importIntegerAddr a 128## 0#

        freezeByteArray mba

    print $ importInteger ba 0## 0## 1#
    print $ importInteger ba 0## 0## 0#

    print $ importInteger ba 5## 29## 1#
    print $ importInteger ba 50## 29## 0#

    print $ importInteger ba 0## 128## 1#
    print $ importInteger ba 0## 128## 0#

    return ()
  where
    b = 2988348162058574136915891421498819466320163312926952423791023078876139
    e = 2351399303373464486466122544523690094744975233415544072992656881240319
    m = 10^(40::Int)

    x = 5328841272400314897981163497728751426
    y = 32052182750761975518649228050096851724

    b1024 = roll (map fromIntegral (take 128 [0x80::Int .. ]))

    rle = map (\x -> (length x, head x)) . group


    roll :: [Word8] -> Integer
    roll = GHC.Base.foldr (\b a -> a `shiftL` 8 .|. fromIntegral b) 0
