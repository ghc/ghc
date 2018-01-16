{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Benchmark all 'Builder' functions.
module Main (main) where

import           Criterion.Main
import           Data.Foldable                         (foldMap)
import           Data.Monoid
import           Prelude                               hiding (words)

import qualified Data.ByteString                       as S
import qualified Data.ByteString.Lazy                  as L

import           Data.ByteString.Builder
import           Data.ByteString.Builder.ASCII
import           Data.ByteString.Builder.Extra         (byteStringCopy,
                                                        byteStringInsert,
                                                        intHost)
import           Data.ByteString.Builder.Internal      (ensureFree)
import           Data.ByteString.Builder.Prim          (BoundedPrim, FixedPrim,
                                                        (>$<))
import qualified Data.ByteString.Builder.Prim          as P
import qualified Data.ByteString.Builder.Prim.Internal as PI

import qualified Blaze.ByteString.Builder          as Blaze
import qualified Blaze.Text                        as Blaze
import qualified "bytestring" Data.ByteString      as OldS
import qualified "bytestring" Data.ByteString.Lazy as OldL

import           Foreign

import System.Random


------------------------------------------------------------------------------
-- Benchmark support
------------------------------------------------------------------------------

countToZero :: Int -> Maybe (Int, Int)
countToZero 0 = Nothing
countToZero n = Just (n, n - 1)


------------------------------------------------------------------------------
-- Benchmark
------------------------------------------------------------------------------

-- input data (NOINLINE to ensure memoization)
----------------------------------------------

-- | Few-enough repetitions to avoid making GC too expensive.
nRepl :: Int
nRepl = 10000

{-# NOINLINE intData #-}
intData :: [Int]
intData = [1..nRepl]

{-# NOINLINE smallIntegerData #-}
smallIntegerData :: [Integer]
smallIntegerData = map fromIntegral intData

{-# NOINLINE largeIntegerData #-}
largeIntegerData :: [Integer]
largeIntegerData = map (* (10 ^ (100 :: Integer))) smallIntegerData


{-# NOINLINE floatData #-}
floatData :: [Float]
floatData = map (\x -> (3.14159 * fromIntegral x) ^ (3 :: Int)) intData

{-# NOINLINE doubleData #-}
doubleData :: [Double]
doubleData = map (\x -> (3.14159 * fromIntegral x) ^ (3 :: Int)) intData

{-# NOINLINE byteStringData #-}
byteStringData :: S.ByteString
byteStringData = S.pack $ map fromIntegral intData

{-# NOINLINE lazyByteStringData #-}
lazyByteStringData :: L.ByteString
lazyByteStringData = case S.splitAt (nRepl `div` 2) byteStringData of
    (bs1, bs2) -> L.fromChunks [bs1, bs2]

{-# NOINLINE byteStringChunksData #-}
byteStringChunksData :: [S.ByteString]
byteStringChunksData = map (S.pack . replicate (4 ) . fromIntegral) intData

{-# NOINLINE oldByteStringChunksData #-}
oldByteStringChunksData :: [OldS.ByteString]
oldByteStringChunksData = map (OldS.pack . replicate (4 ) . fromIntegral) intData


-- benchmark wrappers
---------------------

{-# INLINE benchBlaze #-}
benchBlaze :: String -> a -> (a -> Blaze.Builder) -> Benchmark
benchBlaze name x b =
    bench (name ++" (" ++ show nRepl ++ ")") $
        whnf (OldL.length . Blaze.toLazyByteString . b) x


{-# INLINE benchB #-}
benchB :: String -> a -> (a -> Builder) -> Benchmark
benchB name x b =
    bench (name ++" (" ++ show nRepl ++ ")") $
        whnf (L.length . toLazyByteString . b) x

{-# INLINE benchB' #-}
benchB' :: String -> a -> (a -> Builder) -> Benchmark
benchB' name x b = bench name $ whnf (L.length . toLazyByteString . b) x

{-# INLINE benchBInts #-}
benchBInts :: String -> ([Int] -> Builder) -> Benchmark
benchBInts name = benchB name intData

-- | Benchmark a 'FixedPrim'. Full inlining to enable specialization.
{-# INLINE benchFE #-}
benchFE :: String -> FixedPrim Int -> Benchmark
benchFE name = benchBE name . P.liftFixedToBounded

-- | Benchmark a 'BoundedPrim'. Full inlining to enable specialization.
{-# INLINE benchBE #-}
benchBE :: String -> BoundedPrim Int -> Benchmark
benchBE name e =
  bench (name ++" (" ++ show nRepl ++ ")") $ benchIntEncodingB nRepl e

-- We use this construction of just looping through @n,n-1,..,1@ to ensure that
-- we measure the speed of the encoding and not the speed of generating the
-- values to be encoded.
{-# INLINE benchIntEncodingB #-}
benchIntEncodingB :: Int              -- ^ Maximal 'Int' to write
                  -> BoundedPrim Int  -- ^ 'BoundedPrim' to execute
                  -> IO ()            -- ^ 'IO' action to benchmark
benchIntEncodingB n0 w
  | n0 <= 0   = return ()
  | otherwise = do
      fpbuf <- mallocForeignPtrBytes (n0 * PI.sizeBound w)
      withForeignPtr fpbuf (loop n0) >> return ()
  where
    loop !n !op
      | n <= 0    = return op
      | otherwise = PI.runB w n op >>= loop (n - 1)

hashInt :: Int -> Int
hashInt x = iterate step x !! 10
  where
    step a = e
      where b = (a `xor` 61) `xor` (a `shiftR` 16)
            c = b + (b `shiftL` 3)
            d = c `xor` (c `shiftR` 4)
            e = d * 0x27d4eb2d
            f = e `xor` (e `shiftR` 15)

w :: Int -> Word8
w = fromIntegral

hashWord8 :: Word8 -> Word8
hashWord8 = fromIntegral . hashInt . w

partitionStrict p = nf (S.partition p) . randomStrict $ mkStdGen 98423098
  where randomStrict = fst . S.unfoldrN 10000 (Just . random)

partitionLazy p = nf (L.partition p) . randomLazy $ (0, mkStdGen 98423098)
  where step (k, g)
          | k >= 10000 = Nothing
          | otherwise  = let (x, g') = random g in Just (x, (k + 1, g'))
        randomLazy = L.unfoldr step

easySubstrings, randomSubstrings :: Int -> Int -> (S.ByteString, S.ByteString)
hardSubstrings, pathologicalSubstrings :: Int ->
                                          Int -> (S.ByteString, S.ByteString)

{-# INLINE easySubstrings #-}
easySubstrings n h = (S.replicate n $ w 1,
                      S.replicate h $ w 0)

{-# INLINE randomSubstrings #-}
randomSubstrings n h = (f 48278379 n, f 98403980 h)
  where
    next' g = let (x, g') = next g in (w x, g')
    f g l = fst $ S.unfoldrN l (Just . next') (mkStdGen g)

{-# INLINE hardSubstrings #-}
hardSubstrings n h = (f 48278379 n, f 98403980 h)
  where
    next' g = let (x, g') = next g
              in (w $ x `mod` 4, g')
    f g l = fst $ S.unfoldrN l (Just . next') (mkStdGen g)

{-# INLINE pathologicalSubstrings #-}
pathologicalSubstrings n h =
  (S.replicate n (w 0),
   S.concat . replicate (h `div` n) $ S.replicate (n - 1) (w 0) `S.snoc` w 1)

htmlSubstrings :: S.ByteString -> Int -> Int -> IO (S.ByteString, S.ByteString)
htmlSubstrings s n h =
    do i <- randomRIO (0, l - n)
       return (S.take n . S.drop i $ s', s')
  where
    s' = S.take h s
    l  = S.length s'

-- benchmarks
-------------

sanityCheckInfo :: [String]
sanityCheckInfo =
  [ "Sanity checks:"
  , " lengths of input data: " ++ show
      [ length intData, length floatData, length doubleData
      , length smallIntegerData, length largeIntegerData
      , S.length byteStringData, fromIntegral (L.length lazyByteStringData)
      ]
  ]

main :: IO ()
main = do
  mapM_ putStrLn sanityCheckInfo
  putStrLn ""
  wikiPage <- getDataFileName "wiki-haskell.html" >>= S.readFile
  Criterion.Main.defaultMain
    [ bgroup "Data.ByteString.Builder"
      [ bgroup "Small payload"
        [ benchB' "mempty"        ()  (const mempty)
        , benchB' "ensureFree 8"  ()  (const (ensureFree 8))
        , benchB' "intHost 1"     1   intHost
        ]

      , bgroup "Encoding wrappers"
        [ benchBInts "foldMap word8" $
            foldMap (word8 . fromIntegral)
        , benchBInts "primMapListFixed word8" $
            P.primMapListFixed (fromIntegral >$< P.word8)
        , benchB     "primUnfoldrFixed word8" nRepl $
            P.primUnfoldrFixed (fromIntegral >$< P.word8) countToZero
        , benchB     "primMapByteStringFixed word8" byteStringData $
            P.primMapByteStringFixed P.word8
        , benchB     "primMapLazyByteStringFixed word8" lazyByteStringData $
            P.primMapLazyByteStringFixed P.word8
        ]
      , bgroup "ByteString insertion" $
          let dataName = " byteStringChunks" ++
                         show (S.length (head byteStringChunksData)) ++ "Data"
          in
            [ benchB ("foldMap byteStringInsert" ++ dataName) byteStringChunksData
                (foldMap byteStringInsert)
            , benchB ("foldMap byteString" ++ dataName) byteStringChunksData
                (foldMap byteString)
            , benchB ("foldMap byteStringCopy" ++ dataName) byteStringChunksData
                (foldMap byteStringCopy)
            , benchBlaze ("foldMap Blaze.insertByteString" ++ dataName) oldByteStringChunksData
                (foldMap Blaze.insertByteString)
            , benchBlaze ("foldMap Blaze.fromByteString" ++ dataName) oldByteStringChunksData
                (foldMap Blaze.fromByteString)
            ]

      , bgroup "Non-bounded encodings"
        [ benchB "byteStringHex"           byteStringData     $ byteStringHex
        , benchB "lazyByteStringHex"       lazyByteStringData $ lazyByteStringHex
        , benchB "foldMap floatDec"        floatData          $ foldMap floatDec
        , benchB "foldMap doubleDec"       doubleData         $ foldMap doubleDec
          -- Note that the small data corresponds to the intData pre-converted
          -- to Integer.
        , benchB "foldMap integerDec (small)"                     smallIntegerData        $ foldMap integerDec
        , benchB "foldMap integerDec (large)"                     largeIntegerData        $ foldMap integerDec
        , benchBlaze "foldMap integerDec (small) (blaze-textual)" smallIntegerData        $ foldMap Blaze.integral
        , benchBlaze "foldMap integerDec (large) (blaze-textual)" largeIntegerData        $ foldMap Blaze.integral
        ]
      ]

    , bgroup "substrings"
      [ bgroup "easy"
        [ bench "easy1"    . nf (uncurry S.findSubstrings)
                          $ easySubstrings 1 1000000
        , bench "easy4"    . nf (uncurry S.findSubstrings)
                          $ easySubstrings 4 1000000
        , bench "easy16"   . nf (uncurry S.findSubstrings)
                          $ easySubstrings 16 1000000
        , bench "easy64"   . nf (uncurry S.findSubstrings)
                          $ easySubstrings 64 1000000
        , bench "easy128"  . nf (uncurry S.findSubstrings)
                          $ easySubstrings 128 1000000
        , bench "easy1024" . nf (uncurry S.findSubstrings)
                          $ easySubstrings 1024 1000000
        ]
      , bgroup "random"
        [ bench "random1"    . nf (uncurry S.findSubstrings)
                          $ randomSubstrings 1 1000000
        , bench "random4"    . nf (uncurry S.findSubstrings)
                          $ randomSubstrings 4 1000000
        , bench "random16"   . nf (uncurry S.findSubstrings)
                          $ randomSubstrings 16 1000000
        , bench "random64"   . nf (uncurry S.findSubstrings)
                          $ randomSubstrings 64 1000000
        , bench "random128"  . nf (uncurry S.findSubstrings)
                          $ randomSubstrings 128 1000000
        , bench "random1024" . nf (uncurry S.findSubstrings)
                          $ randomSubstrings 1024 1000000

        ]
      , bgroup "hard"
        [ bench "hard1"    . nf (uncurry S.findSubstrings)
                          $ hardSubstrings 1 1000000
        , bench "hard4"    . nf (uncurry S.findSubstrings)
                          $ hardSubstrings 4 1000000
        , bench "hard16"   . nf (uncurry S.findSubstrings)
                          $ hardSubstrings 16 1000000
        , bench "hard64"   . nf (uncurry S.findSubstrings)
                          $ hardSubstrings 64 1000000
        , bench "hard128"  . nf (uncurry S.findSubstrings)
                          $ hardSubstrings 128 1000000
        , bench "hard1024" . nf (uncurry S.findSubstrings)
                          $ hardSubstrings 1024 1000000
        ]
      , bgroup "pathological"
        [ bench "pathological1"    . nf (uncurry S.findSubstrings)
                          $ pathologicalSubstrings 1 1000000
        , bench "pathological4"    . nf (uncurry S.findSubstrings)
                          $ pathologicalSubstrings 4 1000000
        , bench "pathological16"   . nf (uncurry S.findSubstrings)
                          $ pathologicalSubstrings 16 1000000
        , bench "pathological64"   . nf (uncurry S.findSubstrings)
                          $ pathologicalSubstrings 64 1000000
        , bench "pathological128"  . nf (uncurry S.findSubstrings)
                          $ pathologicalSubstrings 128 1000000
        , bench "pathological1024" . nf (uncurry S.findSubstrings)
                          $ pathologicalSubstrings 1024 1000000
        ]
      , bgroup "html"
        [ bench "html1"    . nfIO . fmap (uncurry S.findSubstrings)
                          $ htmlSubstrings wikiPage 1 1000000
        , bench "html4"    . nfIO . fmap (uncurry S.findSubstrings)
                          $ htmlSubstrings wikiPage 4 1000000
        , bench "html16"   . nfIO . fmap (uncurry S.findSubstrings)
                          $ htmlSubstrings wikiPage 16 1000000
        , bench "html64"   . nfIO . fmap (uncurry S.findSubstrings)
                          $ htmlSubstrings wikiPage 64 1000000
        , bench "html128"  . nfIO . fmap (uncurry S.findSubstrings)
                          $ htmlSubstrings wikiPage 128 1000000
        , bench "html1024" . nfIO . fmap (uncurry S.findSubstrings)
                          $ htmlSubstrings wikiPage 1024 1000000
        ]
      ]

    , bgroup "Data.ByteString.Builder.Prim"
      [ benchFE "char7"      $ toEnum       >$< P.char7
      , benchFE "char8"      $ toEnum       >$< P.char8
      , benchBE "charUtf8"   $ toEnum       >$< P.charUtf8

      -- binary encoding
      , benchFE "int8"       $ fromIntegral >$< P.int8
      , benchFE "word8"      $ fromIntegral >$< P.word8

      -- big-endian
      , benchFE "int16BE"    $ fromIntegral >$< P.int16BE
      , benchFE "int32BE"    $ fromIntegral >$< P.int32BE
      , benchFE "int64BE"    $ fromIntegral >$< P.int64BE

      , benchFE "word16BE"   $ fromIntegral >$< P.word16BE
      , benchFE "word32BE"   $ fromIntegral >$< P.word32BE
      , benchFE "word64BE"   $ fromIntegral >$< P.word64BE

      , benchFE "floatBE"    $ fromIntegral >$< P.floatBE
      , benchFE "doubleBE"   $ fromIntegral >$< P.doubleBE

      -- little-endian
      , benchFE "int16LE"    $ fromIntegral >$< P.int16LE
      , benchFE "int32LE"    $ fromIntegral >$< P.int32LE
      , benchFE "int64LE"    $ fromIntegral >$< P.int64LE

      , benchFE "word16LE"   $ fromIntegral >$< P.word16LE
      , benchFE "word32LE"   $ fromIntegral >$< P.word32LE
      , benchFE "word64LE"   $ fromIntegral >$< P.word64LE

      , benchFE "floatLE"    $ fromIntegral >$< P.floatLE
      , benchFE "doubleLE"   $ fromIntegral >$< P.doubleLE

      -- host-dependent
      , benchFE "int16Host"  $ fromIntegral >$< P.int16Host
      , benchFE "int32Host"  $ fromIntegral >$< P.int32Host
      , benchFE "int64Host"  $ fromIntegral >$< P.int64Host
      , benchFE "intHost"    $ fromIntegral >$< P.intHost

      , benchFE "word16Host" $ fromIntegral >$< P.word16Host
      , benchFE "word32Host" $ fromIntegral >$< P.word32Host
      , benchFE "word64Host" $ fromIntegral >$< P.word64Host
      , benchFE "wordHost"   $ fromIntegral >$< P.wordHost

      , benchFE "floatHost"  $ fromIntegral >$< P.floatHost
      , benchFE "doubleHost" $ fromIntegral >$< P.doubleHost
      ]

    , bgroup "Data.ByteString.Builder.Prim.ASCII"
      [
      -- decimal number
        benchBE "int8Dec"     $ fromIntegral >$< P.int8Dec
      , benchBE "int16Dec"    $ fromIntegral >$< P.int16Dec
      , benchBE "int32Dec"    $ fromIntegral >$< P.int32Dec
      , benchBE "int64Dec"    $ fromIntegral >$< P.int64Dec
      , benchBE "intDec"      $ fromIntegral >$< P.intDec

      , benchBE "word8Dec"    $ fromIntegral >$< P.word8Dec
      , benchBE "word16Dec"   $ fromIntegral >$< P.word16Dec
      , benchBE "word32Dec"   $ fromIntegral >$< P.word32Dec
      , benchBE "word64Dec"   $ fromIntegral >$< P.word64Dec
      , benchBE "wordDec"     $ fromIntegral >$< P.wordDec

      -- hexadecimal number
      , benchBE "word8Hex"    $ fromIntegral >$< P.word8Hex
      , benchBE "word16Hex"   $ fromIntegral >$< P.word16Hex
      , benchBE "word32Hex"   $ fromIntegral >$< P.word32Hex
      , benchBE "word64Hex"   $ fromIntegral >$< P.word64Hex
      , benchBE "wordHex"     $ fromIntegral >$< P.wordHex

      -- fixed-width hexadecimal numbers
      , benchFE "int8HexFixed"     $ fromIntegral >$< P.int8HexFixed
      , benchFE "int16HexFixed"    $ fromIntegral >$< P.int16HexFixed
      , benchFE "int32HexFixed"    $ fromIntegral >$< P.int32HexFixed
      , benchFE "int64HexFixed"    $ fromIntegral >$< P.int64HexFixed

      , benchFE "word8HexFixed"    $ fromIntegral >$< P.word8HexFixed
      , benchFE "word16HexFixed"   $ fromIntegral >$< P.word16HexFixed
      , benchFE "word32HexFixed"   $ fromIntegral >$< P.word32HexFixed
      , benchFE "word64HexFixed"   $ fromIntegral >$< P.word64HexFixed

      , benchFE "floatHexFixed"    $ fromIntegral >$< P.floatHexFixed
      , benchFE "doubleHexFixed"   $ fromIntegral >$< P.doubleHexFixed
      ]
    , bgroup "partition"
      [
        bgroup "strict"
        [
          bench "mostlyTrueFast"  $ partitionStrict (< (w 225))
        , bench "mostlyFalseFast" $ partitionStrict (< (w 10))
        , bench "balancedFast"    $ partitionStrict (< (w 128))

        , bench "mostlyTrueSlow"  $ partitionStrict (\x -> hashWord8 x < w 225)
        , bench "mostlyFalseSlow" $ partitionStrict (\x -> hashWord8 x < w 10)
        , bench "balancedSlow"    $ partitionStrict (\x -> hashWord8 x < w 128)
        ]
      , bgroup "lazy"
        [
          bench "mostlyTrueFast"  $ partitionLazy (< (w 225))
        , bench "mostlyFalseFast" $ partitionLazy (< (w 10))
        , bench "balancedFast"    $ partitionLazy (< (w 128))

        , bench "mostlyTrueSlow"  $ partitionLazy (\x -> hashWord8 x < w 225)
        , bench "mostlyFalseSlow" $ partitionLazy (\x -> hashWord8 x < w 10)
        , bench "balancedSlow"    $ partitionLazy (\x -> hashWord8 x < w 128)
        ]
      ]
    ]
