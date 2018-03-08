{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE UnboxedTuples #-}

-- exercise the 'compareByteArray#' primitive

module Main (main) where

import           Control.Monad
import           Control.Monad.ST
import           Data.List
import           GHC.Exts         (Int (..))
import           GHC.Prim
import           GHC.ST           (ST (ST))
import           GHC.Word         (Word8 (..))
import           Text.Printf

data BA    = BA#   ByteArray#

instance Show BA where
  show xs = "[" ++ intercalate "," (map (printf "0x%02x") (unpack xs)) ++ "]"

instance Eq BA where
  x == y = eqByteArray x 0 (sizeofByteArray x) y 0 (sizeofByteArray y)

instance Ord BA where
  compare x y = ordByteArray x 0 (sizeofByteArray x) y 0 (sizeofByteArray y)

compareByteArrays :: BA -> Int -> BA -> Int -> Int -> Int
compareByteArrays (BA# ba1#) (I# ofs1#) (BA# ba2#) (I# ofs2#) (I# n#)
  = I# (compareByteArrays# ba1# ofs1# ba2# ofs2# n#)

{-
copyByteArray :: BA -> Int -> MBA s -> Int -> Int -> ST s ()
copyByteArray (BA# src#) (I# srcOfs#) (MBA# dest#) (I# destOfs#) (I# n#)
  = ST $ \s -> case copyByteArray# src# srcOfs# dest# destOfs# n# s of
                 s' -> (# s', () #)
-}

indexWord8Array :: BA -> Int -> Word8
indexWord8Array (BA# ba#) (I# i#)
  = W8# (indexWord8Array# ba# i#)

sizeofByteArray :: BA -> Int
sizeofByteArray (BA# ba#) = I# (sizeofByteArray# ba#)


data MBA s = MBA# (MutableByteArray# s)

newByteArray :: Int -> ST s (MBA s)
newByteArray (I# n#)
  = ST $ \s -> case newByteArray# n# s of
                 (# s', mba# #) -> (# s', MBA# mba# #)

writeWord8Array :: MBA s -> Int -> Word8 -> ST s ()
writeWord8Array (MBA# mba#) (I# i#) (W8# j#)
  = ST $ \s -> case writeWord8Array# mba# i# j# s of
                 s' -> (# s', () #)

unsafeFreezeByteArray :: MBA s -> ST s BA
unsafeFreezeByteArray (MBA# mba#)
  = ST $ \s -> case unsafeFreezeByteArray# mba# s of
                 (# s', ba# #) -> (# s', BA# ba# #)

----------------------------------------------------------------------------
-- high-level operations

createByteArray :: Int -> (forall s. MBA s -> ST s ()) -> BA
createByteArray n go = runST $ do
    mba <- newByteArray n
    go mba
    unsafeFreezeByteArray mba

pack :: [Word8] -> BA
pack xs = createByteArray (length xs) $ \mba -> do
    let  go _ [] = pure ()
         go i (y:ys) = do
           writeWord8Array mba i y
           go (i+1) ys
    go 0 xs

unpack :: BA -> [Word8]
unpack ba = go 0
  where
    go i | i < sz = indexWord8Array ba i : go (i+1)
         | otherwise = []
    sz = sizeofByteArray ba

eqByteArray :: BA -> Int -> Int -> BA -> Int -> Int -> Bool
eqByteArray ba1 ofs1 n1 ba2 ofs2 n2
  | n1 /= n2  = False
  | n1 == 0   = True
  | otherwise = compareByteArrays ba1 ofs1 ba2 ofs2 n1 == 0

ordByteArray :: BA -> Int -> Int -> BA -> Int -> Int -> Ordering
ordByteArray ba1 ofs1 n1 ba2 ofs2 n2
  | n == 0 = compare n1 n2
  | otherwise = case compareByteArrays ba1 ofs1 ba2 ofs2 n of
      r | r < 0     -> LT
        | r > 0     -> GT
        | n1 < n2   -> LT
        | n1 > n2   -> GT
        | otherwise -> EQ
  where
    n = n1 `min` n2

main :: IO ()
main = do
    putStrLn "BEGIN"
    -- a couple of low-level tests
    print (compareByteArrays s1 0 s2 0 4 `compare` 0)
    print (compareByteArrays s2 0 s1 0 4 `compare` 0)
    print (compareByteArrays s1 0 s2 0 3 `compare` 0)
    print (compareByteArrays s1 0 s2 1 3 `compare` 0)
    print (compareByteArrays s1 3 s2 2 1 `compare` 0)

    forM_ [(s1,s1),(s1,s2),(s2,s1),(s2,s2)] $ \(x,y) -> do
      print (x == y, compare x y)

    -- realistic test
    print (sort (map pack strs) == map pack (sort strs))

    -- brute-force test
    forM_ [1..15] $ \n -> do
      forM_ [0..rnglen-(n+1)] $ \j -> do
        forM_ [0..rnglen-(n+1)] $ \k -> do
          let iut = compareByteArrays srng j srng k n `compare` 0
              ref = (take n (drop j rng) `compare` take n (drop k rng))
          unless (iut == ref) $
            print ("FAIL",n,j,k,iut,ref)

    putStrLn "END"
  where
    s1, s2 :: BA
    s1 = pack [0xca,0xfe,0xba,0xbe]
    s2 = pack [0xde,0xad,0xbe,0xef]

    strs = let go i xs = case splitAt (i `mod` 5) xs of
                           ([],[]) -> []
                           (y,ys)  -> y : go (i+1) ys
           in go 1 rng

    srng = pack rng

    rnglen = length rng

    rng :: [Word8]
    rng = [ 0xc1, 0x60, 0x31, 0xb6, 0x46, 0x81, 0xa7, 0xc6, 0xa8, 0xf4, 0x1e, 0x5d, 0xb7, 0x7c, 0x0b, 0xcd
          , 0x10, 0xfa, 0xe3, 0xdd, 0xf4, 0x26, 0xf9, 0x50, 0x4b, 0x9c, 0xdf, 0xc4, 0xda, 0xca, 0xc1, 0x60
          , 0x91, 0xf8, 0x70, 0x1a, 0x53, 0x89, 0xf1, 0xd9, 0xee, 0xff, 0x52, 0xb8, 0x1c, 0x5e, 0x25, 0x69
          , 0xd1, 0xa1, 0x08, 0x47, 0x93, 0x89, 0x71, 0x7a, 0xe4, 0x56, 0x24, 0x1b, 0xa1, 0x43, 0x63, 0xc0
          , 0x4d, 0xec, 0x93, 0x30, 0xb7, 0x98, 0x19, 0x23, 0x4e, 0x00, 0x76, 0x7e, 0xf4, 0xcc, 0x8b, 0x92
          , 0x19, 0xc5, 0x3d, 0xf4, 0xa0, 0x4f, 0xe3, 0x64, 0x1b, 0x4e, 0x01, 0xc9, 0xfc, 0x47, 0x3e, 0x16
          , 0xa4, 0x78, 0xdd, 0x12, 0x20, 0xa6, 0x0b, 0xcd, 0x82, 0x06, 0xd0, 0x2a, 0x19, 0x2d, 0x2f, 0xf2
          , 0x8a, 0xf0, 0xc2, 0x2d, 0x0e, 0xfb, 0x39, 0x55, 0xb2, 0xfb, 0x6e, 0xd0, 0xfa, 0xf0, 0x87, 0x57
          , 0x93, 0xa3, 0xae, 0x36, 0x1f, 0xcf, 0x91, 0x45, 0x44, 0x11, 0x62, 0x7f, 0x18, 0x9a, 0xcb, 0x54
          , 0x78, 0x3c, 0x04, 0xbe, 0x3e, 0xd4, 0x2c, 0xbf, 0x73, 0x38, 0x9e, 0xf5, 0xc9, 0xbe, 0xd9, 0xf8
          , 0xe5, 0xf5, 0x41, 0xbb, 0x84, 0x03, 0x2c, 0xe2, 0x0d, 0xe5, 0x8b, 0x1c, 0x75, 0xf7, 0x4c, 0x49
          , 0xfe, 0xac, 0x9f, 0xf4, 0x36, 0xf2, 0xba, 0x5f, 0xc0, 0xda, 0x24, 0xfc, 0x10, 0x61, 0xf0, 0xb6
          , 0xa7, 0xc7, 0xba, 0xc6, 0xb0, 0x41, 0x04, 0x8c, 0xd0, 0xe8, 0x48, 0x41, 0x38, 0xa4, 0x84, 0x21
          , 0xb6, 0xb1, 0x21, 0x33, 0x58, 0xf2, 0xa5, 0xe5, 0x73, 0xf2, 0xd7, 0xbc, 0xc7, 0x7e, 0x86, 0xee
          , 0x81, 0xb1, 0xcd, 0x42, 0xc0, 0x2c, 0xd0, 0xa0, 0x8d, 0xb5, 0x4a, 0x5b, 0xc1, 0xfe, 0xcc, 0x92
          , 0x59, 0xf4, 0x71, 0x96, 0x58, 0x6a, 0xb6, 0xa2, 0xf7, 0x67, 0x76, 0x01, 0xc5, 0x8b, 0xc9, 0x6f
          , 0x38, 0x93, 0xf3, 0xaa, 0x89, 0xf7, 0xb2, 0x2a, 0x0f, 0x19, 0x7b, 0x48, 0xbe, 0x86, 0x37, 0xd1
          , 0x30, 0xfa, 0xce, 0x72, 0xf4, 0x25, 0x64, 0xee, 0xde, 0x3a, 0x5c, 0x02, 0x32, 0xe6, 0x31, 0x3a
          , 0x4b, 0x18, 0x47, 0x30, 0xa4, 0x2c, 0xf8, 0x4d, 0xc5, 0xee, 0x0b, 0x9c, 0x75, 0x43, 0x2a, 0xf9
          ]
