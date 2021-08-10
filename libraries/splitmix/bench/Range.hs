-- http://www.pcg-random.org/posts/bounded-rands.html
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
module Main where

import Data.Bits
import Data.Bits.Compat
import Data.List        (unfoldr)
import Data.Word        (Word32, Word64)

import qualified System.Random.SplitMix32 as SM

#if defined(__GHCJS__)
#else
import System.Clock (Clock (Monotonic), getTime, toNanoSecs)
import Text.Printf  (printf)
#endif

main :: IO ()
main = do
    gen <- SM.newSMGen

    -- bench gen (\g h -> R (0, pred h) g)
    bench gen classicMod
    bench gen intMult
    bench gen bitmaskWithRejection

bench :: g -> (g -> Word32 -> (Word32, g)) -> IO ()
bench gen next = do
    print $ take 70 $ unfoldr (\g -> Just (next g 10)) gen
    clocked $ do
        let x = sumOf next gen
        print x

sumOf :: (g -> Word32 -> (Word32, g)) -> g -> Word32
sumOf next = go 0 2
  where
    go !acc !n g | n > 0xfffff = acc
                 | otherwise    = let (w, g') = next g n in go (acc + w) (succ n) g'

classicMod :: SM.SMGen -> Word32 -> (Word32, SM.SMGen)
classicMod g h =
    let (w32, g') = SM.nextWord32 g in (w32 `mod` h, g')


-- @
-- uint32_t bounded_rand(rng_t& rng, uint32_t range) {
--     uint32_t x = rng();
--     uint64_t m = uint64_t(x) * uint64_t(range);
--     return m >> 32;
-- }
-- @
--
intMult :: SM.SMGen -> Word32 -> (Word32, SM.SMGen)
intMult g h =
    (fromIntegral $ (fromIntegral w32 * fromIntegral h :: Word64) `shiftR` 32, g')
  where
    (w32, g') = SM.nextWord32 g

-- @
-- uint32_t bounded_rand(rng_t& rng, uint32_t range) {
--     uint32_t mask = ~uint32_t(0);
--     --range;
--     mask >>= __builtin_clz(range|1);
--     uint32_t x;
--     do {
--         x = rng() & mask;
--     } while (x > range);
--     return x;
-- }
-- @@
bitmaskWithRejection :: SM.SMGen -> Word32 -> (Word32, SM.SMGen)
bitmaskWithRejection g0 range = go g0
  where
    mask = complement zeroBits `shiftR` countLeadingZeros (range .|. 1)
    go g = let (x, g') = SM.nextWord32 g
               x' = x .&. mask
           in if x' >= range
              then go g'
              else (x', g')

-------------------------------------------------------------------------------
-- Poor man benchmarking with GHC and GHCJS
-------------------------------------------------------------------------------

clocked :: IO () -> IO ()
#if defined(__GHCJS__)
clocked action = do
    start
    action
    stop

foreign import javascript unsafe
    "console.time('loop');"
    start :: IO ()

foreign import javascript unsafe
    "console.timeEnd('loop');"
    stop :: IO ()
#else
clocked action =  do
    start <- getTime Monotonic
    action
    end <- getTime Monotonic
    printf "loop: %.03fms\n"
        $ fromIntegral (toNanoSecs (end - start))
        / (1e6 :: Double)
#endif
