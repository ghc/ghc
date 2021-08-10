-- |
-- /SplitMix/ is a splittable pseudorandom number generator (PRNG) that is quite fast.
--
-- This is 32bit variant (original one is 32 bit).
--
-- You __really don't want to use this one__.
--
--  Note: This module supports all GHCs since GHC-7.0.4,
--  but GHC-7.0 and GHC-7.2 have slow implementation, as there
--  are no native 'popCount'.
--
{-# LANGUAGE CPP         #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module System.Random.SplitMix32 (
    SMGen,
    nextWord32,
    nextWord64,
    nextTwoWord32,
    nextInt,
    nextDouble,
    nextFloat,
    nextInteger,
    splitSMGen,
    -- * Generation
    bitmaskWithRejection32,
    bitmaskWithRejection32',
    bitmaskWithRejection64,
    bitmaskWithRejection64',
    -- * Initialisation
    mkSMGen,
    initSMGen,
    newSMGen,
    seedSMGen,
    seedSMGen',
    unseedSMGen,
    ) where

import Data.Bits             (complement, shiftL, shiftR, xor, (.&.), (.|.))
import Data.Bits.Compat
       (countLeadingZeros, finiteBitSize, popCount, zeroBits)
import Data.IORef            (IORef, atomicModifyIORef, newIORef)
import Data.Word             (Word32, Word64)
import System.IO.Unsafe      (unsafePerformIO)

import System.Random.SplitMix.Init

#if defined(__HUGS__) || !MIN_VERSION_base(4,8,0)
import Data.Word (Word)
#endif

#ifndef __HUGS__
import Control.DeepSeq (NFData (..))
#endif

-- $setup
-- >>> import Text.Read (readMaybe)
-- >>> import Data.List (unfoldr)
-- >>> import Text.Printf (printf)

-------------------------------------------------------------------------------
-- Generator
-------------------------------------------------------------------------------

-- | SplitMix generator state.
data SMGen = SMGen {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32 -- seed and gamma; gamma is odd
  deriving Show

#ifndef __HUGS__
instance NFData SMGen where
    rnf (SMGen _ _) = ()
#endif

-- |
--
-- >>> readMaybe "SMGen 1 1" :: Maybe SMGen
-- Just (SMGen 1 1)
--
-- >>> readMaybe "SMGen 1 2" :: Maybe SMGen
-- Nothing
--
-- >>> readMaybe (show (mkSMGen 42)) :: Maybe SMGen
-- Just (SMGen 142593372 1604540297)
--
instance Read SMGen where
    readsPrec d r =  readParen (d > 10) (\r0 ->
        [ (SMGen seed gamma, r3)
        | ("SMGen", r1) <- lex r0
        , (seed, r2) <- readsPrec 11 r1
        , (gamma, r3) <- readsPrec 11 r2
        , odd gamma
        ]) r

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- | Generate a 'Word32'.
--
-- >>> take 3 $ map (printf "%x") $ unfoldr (Just . nextWord32) (mkSMGen 1337) :: [String]
-- ["e0cfe722","a6ced0f0","c3a6d889"]
--
nextWord32 :: SMGen -> (Word32, SMGen)
nextWord32 (SMGen seed gamma) = (mix32 seed', SMGen seed' gamma)
  where
    seed' = seed + gamma

-- | Generate a 'Word64', by generating to 'Word32's.
nextWord64 :: SMGen -> (Word64, SMGen)
nextWord64 s0 = (fromIntegral w0 `shiftL` 32 .|. fromIntegral w1,  s2)
  where
    (w0, s1) = nextWord32 s0
    (w1, s2) = nextWord32 s1

-- | Generate two 'Word32'.
nextTwoWord32 :: SMGen -> (Word32, Word32, SMGen)
nextTwoWord32 s0 = (w0, w1, s2) where
    (w0, s1) = nextWord32 s0
    (w1, s2) = nextWord32 s1

-- | Generate an 'Int'.
nextInt :: SMGen -> (Int, SMGen)
nextInt g | isBigInt  = int64
          | otherwise = int32
  where
    int32 = case nextWord32 g of
        (w, g') -> (fromIntegral w, g')
    int64 = case nextWord64 g of
        (w, g') -> (fromIntegral w, g')

isBigInt :: Bool
isBigInt = finiteBitSize (undefined :: Int) > 32

-- | Generate a 'Double' in @[0, 1)@ range.
--
-- >>> take 8 $ map (printf "%0.3f") $ unfoldr (Just . nextDouble) (mkSMGen 1337) :: [String]
-- ["0.878","0.764","0.063","0.845","0.262","0.490","0.176","0.544"]
--
nextDouble :: SMGen -> (Double, SMGen)
nextDouble g = case nextWord64 g of
    (w64, g') -> (fromIntegral (w64 `shiftR` 11) * doubleUlp, g')

-- | Generate a 'Float' in @[0, 1)@ range.
--
-- >>> take 8 $ map (printf "%0.3f") $ unfoldr (Just . nextFloat) (mkSMGen 1337) :: [String]
-- ["0.878","0.652","0.764","0.631","0.063","0.180","0.845","0.645"]
--
nextFloat :: SMGen -> (Float, SMGen)
nextFloat g = case nextWord32 g of
    (w32, g') -> (fromIntegral (w32 `shiftR` 8) * floatUlp, g')

-- | Generate an 'Integer' in closed @[x, y]@ range.
nextInteger :: Integer -> Integer -> SMGen -> (Integer, SMGen)
nextInteger lo hi g = case compare lo hi of
    LT -> let (i, g') = nextInteger' (hi - lo) g in (i + lo, g')
    EQ -> (lo, g)
    GT -> let (i, g') = nextInteger' (lo - hi) g in (i + hi, g')

-- invariant: first argument is positive
-- Essentially bitmaskWithRejection but for Integers.
--
nextInteger' :: Integer -> SMGen -> (Integer, SMGen)
nextInteger' range = loop
  where
    leadMask :: Word32
    restDigits :: Word
    (leadMask, restDigits) = go 0 range where
        go :: Word -> Integer -> (Word32, Word)
        go n x | x < two32 = (complement zeroBits `shiftR` countLeadingZeros (fromInteger x :: Word32), n)
               | otherwise = go (n + 1) (x `shiftR` 32)

    generate :: SMGen -> (Integer, SMGen)
    generate g0 =
        let (x, g') = nextWord32 g0
            x' = x .&. leadMask
        in go (fromIntegral x') restDigits g'
      where
        go :: Integer -> Word -> SMGen -> (Integer, SMGen)
        go acc 0 g = acc `seq` (acc, g)
        go acc n g =
            let (x, g') = nextWord32 g
            in go (acc * two32 + fromIntegral x) (n - 1) g'

    loop g = let (x, g') = generate g
             in if x > range
                then loop g'
                else (x, g')

two32 :: Integer
two32 = 2 ^ (32 :: Int)

-------------------------------------------------------------------------------
-- Splitting
-------------------------------------------------------------------------------

-- | Split a generator into a two uncorrelated generators.
splitSMGen :: SMGen -> (SMGen, SMGen)
splitSMGen (SMGen seed gamma) =
    (SMGen seed'' gamma, SMGen (mix32 seed') (mixGamma seed''))
  where
    seed'  = seed + gamma
    seed'' = seed' + gamma

-------------------------------------------------------------------------------
-- Algorithm
-------------------------------------------------------------------------------

-- | (1 + sqrt 5) / 2 * (2 ^^ bits)
goldenGamma :: Word32
goldenGamma = 0x9e3779b9

floatUlp :: Float
floatUlp =  1.0 / fromIntegral (1 `shiftL` 24 :: Word32)

doubleUlp :: Double
doubleUlp =  1.0 / fromIntegral (1 `shiftL` 53 :: Word64)

#if defined(__GHCJS__) && defined(OPTIMISED_MIX32)
-- JavaScript Foreign Function Interface
-- https://github.com/ghcjs/ghcjs/blob/master/doc/foreign-function-interface.md

foreign import javascript unsafe
    "var x0 = $1 ^ $1 >>> 16; var x1 = x0 & 0xffff; var x2 = (((x0 >>> 16 & 0xffff) * 0x0000ca6b + x1 * 0x000085eb & 0xffff) << 16) + x1 * 0x0000ca6b; var x3 = x2 ^ x2 >>> 13; var x4 = x3 & 0xffff; var x5 = (((x3 >>> 16 & 0xffff) * 0x0000ae35 + x4 * 0x0000c2b2 & 0xffff) << 16) + x4 * 0x0000ae35; $r = (x5 ^ x5 >>> 16) | 0;"
    mix32 :: Word32 -> Word32

foreign import javascript unsafe
    "var x0 = $1 ^ $1 >>> 16; var x1 = x0 & 0xffff; var x2 = (((x0 >>> 16 & 0xffff) * 0x00006ccb + x1 * 0x000069ad & 0xffff) << 16) + x1 * 0x00006ccb; var x3 = x2 ^ x2 >>> 13; var x4 = x3 & 0xffff; var x5 = (((x3 >>> 16 & 0xffff) * 0x0000b5b3 + x4 * 0x0000cd9a & 0xffff) << 16) + x4 * 0x0000b5b3; $r = (x5 ^ x5 >>> 16) | 0;"
    mix32variant13 :: Word32 -> Word32

#else
mix32 :: Word32 -> Word32
mix32 z0 =
   -- MurmurHash3Mixer 32bit
    let z1 = shiftXorMultiply 16 0x85ebca6b z0
        z2 = shiftXorMultiply 13 0xc2b2ae35 z1
        z3 = shiftXor 16 z2
    in z3

-- used only in mixGamma
mix32variant13 :: Word32 -> Word32
mix32variant13 z0 =
   -- See avalanche "executable"
    let z1 = shiftXorMultiply 16 0x69ad6ccb z0
        z2 = shiftXorMultiply 13 0xcd9ab5b3 z1
        z3 = shiftXor 16 z2
    in z3

shiftXor :: Int -> Word32 -> Word32
shiftXor n w = w `xor` (w `shiftR` n)

shiftXorMultiply :: Int -> Word32 -> Word32 -> Word32
shiftXorMultiply n k w = shiftXor n w * k
#endif

mixGamma :: Word32 -> Word32
mixGamma z0 =
    let z1 = mix32variant13 z0 .|. 1             -- force to be odd
        n  = popCount (z1 `xor` (z1 `shiftR` 1))
    -- see: http://www.pcg-random.org/posts/bugs-in-splitmix.html
    -- let's trust the text of the paper, not the code.
    in if n >= 12
        then z1
        else z1 `xor` 0xaaaaaaaa

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

-- | /Bitmask with rejection/ method of generating subrange of 'Word32'.
--
-- @bitmaskWithRejection32 w32@ generates random numbers in closed-open
-- range of @[0, w32)@.
--
bitmaskWithRejection32 :: Word32 -> SMGen -> (Word32, SMGen)
bitmaskWithRejection32 0 = error "bitmaskWithRejection32 0"
bitmaskWithRejection32 n = bitmaskWithRejection32' (n - 1)
{-# INLINEABLE bitmaskWithRejection32 #-}

-- | /Bitmask with rejection/ method of generating subrange of 'Word64'.
--
-- @bitmaskWithRejection64 w64@ generates random numbers in closed-open
-- range of @[0, w64)@.
--
-- >>> take 20 $ unfoldr (Just . bitmaskWithRejection64 5) (mkSMGen 1337)
-- [0,2,4,2,1,4,2,4,2,2,3,0,3,2,2,2,3,1,2,2]
--
bitmaskWithRejection64 :: Word64 -> SMGen -> (Word64, SMGen)
bitmaskWithRejection64 0 = error "bitmaskWithRejection64 0"
bitmaskWithRejection64 n = bitmaskWithRejection64' (n - 1)
{-# INLINEABLE bitmaskWithRejection64 #-}

-- | /Bitmask with rejection/ method of generating subrange of 'Word32'.
--
-- @bitmaskWithRejection32' w32@ generates random numbers in closed-closed
-- range of @[0, w32]@.
--
-- @since 0.0.4
bitmaskWithRejection32' :: Word32 -> SMGen -> (Word32, SMGen)
bitmaskWithRejection32' range = go where
    mask = complement zeroBits `shiftR` countLeadingZeros (range .|. 1)
    go g = let (x, g') = nextWord32 g
               x' = x .&. mask
           in if x' > range
              then go g'
              else (x', g')
{-# INLINEABLE bitmaskWithRejection32' #-}

-- | /Bitmask with rejection/ method of generating subrange of 'Word64'.
--
-- @bitmaskWithRejection64' w64@ generates random numbers in closed-closed
-- range of @[0, w64]@.
--
-- >>> take 20 $ unfoldr (Just . bitmaskWithRejection64' 5) (mkSMGen 1337)
-- [0,2,4,2,1,4,2,4,5,5,2,2,5,3,5,0,3,2,2,2]
--
-- @since 0.0.4
bitmaskWithRejection64' :: Word64 -> SMGen -> (Word64, SMGen)
bitmaskWithRejection64' range = go where
    mask = complement zeroBits `shiftR` countLeadingZeros (range .|. 1)
    go g = let (x, g') = nextWord64 g
               x' = x .&. mask
           in if x' > range
              then go g'
              else (x', g')
{-# INLINEABLE bitmaskWithRejection64' #-}

-------------------------------------------------------------------------------
-- Initialisation
-------------------------------------------------------------------------------

-- | Create 'SMGen' using seed and gamma.
--
-- >>> seedSMGen 2 2
-- SMGen 2 3
--
seedSMGen
    :: Word32 -- ^ seed
    -> Word32 -- ^ gamma
    -> SMGen
seedSMGen seed gamma = SMGen seed (gamma .|. 1)

-- | Like 'seedSMGen' but takes a pair.
seedSMGen' :: (Word32, Word32) -> SMGen
seedSMGen' = uncurry seedSMGen

-- | Extract current state of 'SMGen'.
unseedSMGen :: SMGen -> (Word32, Word32)
unseedSMGen (SMGen seed gamma) = (seed, gamma)

-- | Preferred way to deterministically construct 'SMGen'.
--
-- >>> mkSMGen 42
-- SMGen 142593372 1604540297
--
mkSMGen :: Word32 -> SMGen
mkSMGen s = SMGen (mix32 s) (mixGamma (s + goldenGamma))

-- | Initialize 'SMGen' using entropy available on the system (time, ...)
initSMGen :: IO SMGen
initSMGen = fmap mkSMGen initialSeed'

-- | Derive a new generator instance from the global 'SMGen' using 'splitSMGen'.
newSMGen :: IO SMGen
newSMGen = atomicModifyIORef theSMGen splitSMGen

theSMGen :: IORef SMGen
theSMGen = unsafePerformIO $ initSMGen >>= newIORef
{-# NOINLINE theSMGen #-}

initialSeed' :: IO Word32
initialSeed' = do
    w64 <- initialSeed
    return (fromIntegral (shiftR w64 32) `xor` fromIntegral w64)
