-- |
-- /SplitMix/ is a splittable pseudorandom number generator (PRNG) that is quite fast.
--
-- Guy L. Steele, Jr., Doug Lea, and Christine H. Flood. 2014.
--  Fast splittable pseudorandom number generators. In Proceedings
--  of the 2014 ACM International Conference on Object Oriented
--  Programming Systems Languages & Applications (OOPSLA '14). ACM,
--  New York, NY, USA, 453-472. DOI:
--  <https://doi.org/10.1145/2660193.2660195>
--
--  The paper describes a new algorithm /SplitMix/ for /splittable/
--  pseudorandom number generator that is quite fast: 9 64 bit arithmetic/logical
--  operations per 64 bits generated.
--
--  /SplitMix/ is tested with two standard statistical test suites (DieHarder and
--  TestU01, this implementation only using the former) and it appears to be
--  adequate for "everyday" use, such as Monte Carlo algorithms and randomized
--  data structures where speed is important.
--
--  In particular, it __should not be used for cryptographic or security applications__,
--  because generated sequences of pseudorandom values are too predictable
--  (the mixing functions are easily inverted, and two successive outputs
--  suffice to reconstruct the internal state).
--
--  Note: This module supports all GHCs since GHC-7.0.4,
--  but GHC-7.0 and GHC-7.2 have slow implementation, as there
--  are no native 'popCount'.
--
{-# LANGUAGE CPP          #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy  #-}
#endif
module System.Random.SplitMix (
    SMGen,
    nextWord64,
    nextWord32,
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
import Data.Bits.Compat      (countLeadingZeros, popCount, zeroBits)
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
data SMGen = SMGen !Word64 !Word64 -- seed and gamma; gamma is odd
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
-- Just (SMGen 9297814886316923340 13679457532755275413)
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

-- | Generate a 'Word64'.
--
-- >>> take 3 $ map (printf "%x") $ unfoldr (Just . nextWord64) (mkSMGen 1337) :: [String]
-- ["b5c19e300e8b07b3","d600e0e216c0ac76","c54efc3b3cc5af29"]
--
nextWord64 :: SMGen -> (Word64, SMGen)
nextWord64 (SMGen seed gamma) = (mix64 seed', SMGen seed' gamma)
  where
    seed' = seed `plus` gamma

-- | Generate 'Word32' by truncating 'nextWord64'.
--
-- @since 0.0.3
nextWord32 :: SMGen -> (Word32, SMGen)
nextWord32 g =
#ifdef __HUGS__
    (fromIntegral $ w64 .&. 0xffffffff, g')
#else
    (fromIntegral w64, g')
#endif
  where
    (w64, g') = nextWord64 g

-- | Generate two 'Word32'.
--
-- @since 0.0.3
nextTwoWord32 :: SMGen -> (Word32, Word32, SMGen)
nextTwoWord32 g =
#ifdef __HUGS__
    (fromIntegral $ w64 `shiftR` 32, fromIntegral $ w64 .&. 0xffffffff, g')
#else
    (fromIntegral $ w64 `shiftR` 32, fromIntegral w64, g')
#endif
  where
    (w64, g') = nextWord64 g

-- | Generate an 'Int'.
nextInt :: SMGen -> (Int, SMGen)
nextInt g = case nextWord64 g of
#ifdef __HUGS__
    (w64, g') -> (fromIntegral $ w64 `shiftR` 32, g')
#else
    (w64, g') -> (fromIntegral w64, g')
#endif

-- | Generate a 'Double' in @[0, 1)@ range.
--
-- >>> take 8 $ map (printf "%0.3f") $ unfoldr (Just . nextDouble) (mkSMGen 1337) :: [String]
-- ["0.710","0.836","0.771","0.409","0.297","0.527","0.589","0.067"]
--
nextDouble :: SMGen -> (Double, SMGen)
nextDouble g = case nextWord64 g of
    (w64, g') -> (fromIntegral (w64 `shiftR` 11) * doubleUlp, g')

-- | Generate a 'Float' in @[0, 1)@ range.
--
-- >>> take 8 $ map (printf "%0.3f") $ unfoldr (Just . nextFloat) (mkSMGen 1337) :: [String]
-- ["0.057","0.089","0.237","0.383","0.680","0.320","0.826","0.007"]
--
-- @since 0.0.3
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
    leadMask :: Word64
    restDigits :: Word
    (leadMask, restDigits) = go 0 range where
        go :: Word -> Integer -> (Word64, Word)
        go n x | x < two64 = (complement zeroBits `shiftR` countLeadingZeros (fromInteger x :: Word64), n)
               | otherwise = go (n + 1) (x `shiftR` 64)

    generate :: SMGen -> (Integer, SMGen)
    generate g0 =
        let (x, g') = nextWord64 g0
            x' = x .&. leadMask
        in go (fromIntegral x') restDigits g'
      where
        go :: Integer -> Word -> SMGen -> (Integer, SMGen)
        go acc 0 g = acc `seq` (acc, g)
        go acc n g =
            let (x, g') = nextWord64 g
            in go (acc * two64 + fromIntegral x) (n - 1) g'

    loop g = let (x, g') = generate g
             in if x > range
                then loop g'
                else (x, g')

two64 :: Integer
two64 = 2 ^ (64 :: Int)

-------------------------------------------------------------------------------
-- Splitting
-------------------------------------------------------------------------------

-- | Split a generator into a two uncorrelated generators.
splitSMGen :: SMGen -> (SMGen, SMGen)
splitSMGen (SMGen seed gamma) =
    (SMGen seed'' gamma, SMGen (mix64 seed') (mixGamma seed''))
  where
    seed'  = seed `plus` gamma
    seed'' = seed' `plus` gamma

-------------------------------------------------------------------------------
-- Algorithm
-------------------------------------------------------------------------------

goldenGamma :: Word64
goldenGamma = 0x9e3779b97f4a7c15

floatUlp :: Float
floatUlp =  1.0 / fromIntegral (1 `shiftL` 24 :: Word32)

doubleUlp :: Double
doubleUlp =  1.0 / fromIntegral (1 `shiftL` 53 :: Word64)

-- Note: in JDK implementations the mix64 and mix64variant13
-- (which is inlined into mixGamma) are swapped.
mix64 :: Word64 -> Word64
mix64 z0 =
   -- MurmurHash3Mixer
    let z1 = shiftXorMultiply 33 0xff51afd7ed558ccd z0
        z2 = shiftXorMultiply 33 0xc4ceb9fe1a85ec53 z1
        z3 = shiftXor 33 z2
    in z3

-- used only in mixGamma
mix64variant13 :: Word64 -> Word64
mix64variant13 z0 =
   -- Better Bit Mixing - Improving on MurmurHash3's 64-bit Finalizer
   -- http://zimbry.blogspot.fi/2011/09/better-bit-mixing-improving-on.html
   --
   -- Stafford's Mix13
    let z1 = shiftXorMultiply 30 0xbf58476d1ce4e5b9 z0 -- MurmurHash3 mix constants
        z2 = shiftXorMultiply 27 0x94d049bb133111eb z1
        z3 = shiftXor 31 z2
    in z3

mixGamma :: Word64 -> Word64
mixGamma z0 =
    let z1 = mix64variant13 z0 .|. 1             -- force to be odd
        n  = popCount (z1 `xor` (z1 `shiftR` 1))
    -- see: http://www.pcg-random.org/posts/bugs-in-splitmix.html
    -- let's trust the text of the paper, not the code.
    in if n >= 24
        then z1
        else z1 `xor` 0xaaaaaaaaaaaaaaaa

shiftXor :: Int -> Word64 -> Word64
shiftXor n w = w `xor` (w `shiftR` n)

shiftXorMultiply :: Int -> Word64 -> Word64 -> Word64
shiftXorMultiply n k w = shiftXor n w `mult` k


-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

-- | /Bitmask with rejection/ method of generating subrange of 'Word32'.
--
-- @bitmaskWithRejection32 w32@ generates random numbers in closed-open
-- range of @[0, w32)@.
--
-- @since 0.0.3
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
-- [3,1,4,1,2,3,1,1,0,3,4,2,3,0,2,3,3,4,1,0]
--
-- @since 0.0.3
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
-- [3,1,4,1,2,3,1,1,0,3,4,5,2,3,0,2,3,5,3,4]
--
-- @since 0.0.4
bitmaskWithRejection64' :: Word64 -> SMGen -> (Word64, SMGen)
bitmaskWithRejection64' range = go where
    mask = complement zeroBits `shiftR` countLeadingZeros range
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
    :: Word64 -- ^ seed
    -> Word64 -- ^ gamma
    -> SMGen
seedSMGen seed gamma = SMGen seed (gamma .|. 1)

-- | Like 'seedSMGen' but takes a pair.
seedSMGen' :: (Word64, Word64) -> SMGen
seedSMGen' = uncurry seedSMGen

-- | Extract current state of 'SMGen'.
unseedSMGen :: SMGen -> (Word64, Word64)
unseedSMGen (SMGen seed gamma) = (seed, gamma)

-- | Preferred way to deterministically construct 'SMGen'.
--
-- >>> mkSMGen 42
-- SMGen 9297814886316923340 13679457532755275413
--
mkSMGen :: Word64 -> SMGen
mkSMGen s = SMGen (mix64 s) (mixGamma (s `plus` goldenGamma))

-- | Initialize 'SMGen' using entropy available on the system (time, ...)
initSMGen :: IO SMGen
initSMGen = fmap mkSMGen initialSeed

-- | Derive a new generator instance from the global 'SMGen' using 'splitSMGen'.
newSMGen :: IO SMGen
newSMGen = atomicModifyIORef theSMGen splitSMGen

theSMGen :: IORef SMGen
theSMGen = unsafePerformIO $ initSMGen >>= newIORef
{-# NOINLINE theSMGen #-}

-------------------------------------------------------------------------------
-- Hugs
-------------------------------------------------------------------------------

mult, plus :: Word64 -> Word64 -> Word64
#ifndef __HUGS__
mult = (*)
plus = (+)
#else
-- Hugs defines:
--
--    x * y         = fromInteger (toInteger x * toInteger y)
--    x + y         = fromInteger (toInteger x + toInteger y)
--
-- which obviously overflows in our use cases, as fromInteger doesn't truncate
--
mult x y = fromInteger ((toInteger x * toInteger y) `mod` 18446744073709551616)
plus x y = fromInteger ((toInteger x + toInteger y) `mod` 18446744073709551616)
#endif
