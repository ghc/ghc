-- !!! Bug # 7600.
-- The LLVM backend can be tricky to get right with floating point constants
-- and GHC. See Note [LLVM Float Types] in compiler/llvmGen/Llvm/Types.hs for
-- why this is.
--
-- Two issues to watch for (that this bug tries to track):
--
-- 1) We need to narrow a double to a float but then expand back out (so that
-- we end up with the precision of a float but in double precision byte form).
-- GHC seems to optimize this away for some ways of doing this.
--
-- 2) The 'realToFrac' method returns different results at the byte level
-- depending on if optimisations are on or off. We use the double2float and
-- float2Double methods instead as they don't suffer from this.
-- 
-- Also worth looking at ticket # 3676 about issues with 'realToFrac'.
module Main (main) where

import T7600_A

-- a fp constant that requires double precision, but we only use a single
-- precision type.
-- expected output: float 0x7FF0000000000000
float_number :: Float
float_number = 1.82173691287639817263897126389712638972163e+300

-- as above but use double precision so we can represent it.
-- expected output: double 0x7E45C3163C1ACF96
double_number :: Double
double_number = 1.82173691287639817263897126389712638972163e+300

-- Test run
main :: IO ()
main = test_run float_number double_number



-- XXX: We don't run below, but it can be useful to test how the optimizer is
-- running... the NOINLINE pragmas are needed below generally, but often not
-- for Bug31_A as the constant is in a different module...

-- -- Test run
-- test_run' :: Float -> Double -> IO ()
-- test_run' float_number double_number = do
--     print $ dToStr double_number
--     print $ dToStr (widen $ narrow double_number)
--     print $ dToStr (widen' $ narrow' double_number)
--     let dd = case double_number of { (D# x) -> x }
--     print $ dToStr (D# (float2Double# (double2Float# dd)))
--
-- -- use standard Haskell functions for type conversion... which are kind of
-- -- insane (see ticket # 3676) [these fail when -O0 is used...]
-- {-# NOINLINE narrow #-}
-- narrow :: Double -> Float
-- narrow = realToFrac
-- 
-- {-# NOINLINE widen #-}
-- widen :: Float -> Double
-- widen = realToFrac
-- 
-- -- use GHC specific functions which work as expected [work for both -O0 and -O]
-- {-# NOINLINE narrow' #-}
-- narrow' :: Double -> Float
-- narrow' = double2Float
-- 
-- {-# NOINLINE widen' #-}
-- widen' :: Float -> Double
-- widen' = float2Double
-- 
-- doubleToBytes :: Double -> [Int]
-- doubleToBytes d
--    = runST (do
--         arr <- newArray_ ((0::Int),7)
--         writeArray arr 0 d
--         arr <- castDoubleToWord8Array arr
--         i0 <- readArray arr 0
--         i1 <- readArray arr 1
--         i2 <- readArray arr 2
--         i3 <- readArray arr 3
--         i4 <- readArray arr 4
--         i5 <- readArray arr 5
--         i6 <- readArray arr 6
--         i7 <- readArray arr 7
--         return (map fromIntegral [i0,i1,i2,i3,i4,i5,i6,i7])
--      )
-- 
-- castFloatToWord8Array :: STUArray s Int Float -> ST s (STUArray s Int Word8)
-- castFloatToWord8Array = castSTUArray
-- 
-- castDoubleToWord8Array :: STUArray s Int Double -> ST s (STUArray s Int Word8)
-- castDoubleToWord8Array = castSTUArray
-- 
-- dToStr :: Double -> String
-- dToStr d
--   = let bs     = doubleToBytes d
--         hex d' = case showHex d' "" of
--                      []    -> error "dToStr: too few hex digits for float"
--                      [x]   -> ['0',x]
--                      [x,y] -> [x,y]
--                      _     -> error "dToStr: too many hex digits for float"
-- 
--         str  = map toUpper $ concat . fixEndian . (map hex) $ bs
--     in  "0x" ++ str
-- 
-- fixEndian :: [a] -> [a]
-- -- #ifdef WORDS_BIGENDIAN
-- -- fixEndian = id
-- -- #else
-- fixEndian = reverse
-- -- #endif

