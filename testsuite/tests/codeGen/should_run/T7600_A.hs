-- !!! Bug # 7600.
-- See file T7600 for main description.
{-# LANGUAGE CPP #-}
module T7600_A (test_run) where

import Control.Monad.ST
import Data.Array.Unsafe( castSTUArray )
import Data.Array.ST hiding( castSTUArray )
import Data.Char
import Data.Word
import Numeric

import GHC.Float

-- Test run
test_run :: Float -> Double -> IO ()
test_run float_number double_number = do
    print $ dToStr double_number
    -- XXX: Below is the bad code due to changing with optimisation.
    -- print $ dToStr (widen $ narrow double_number)
    print $ dToStr (widen' $ narrow' double_number)

-- use standard Haskell functions for type conversion... which are kind of
-- insane (see ticket # 3676) [these fail when -O0 is used...]
narrow :: Double -> Float
{-# NOINLINE narrow #-}
narrow = realToFrac

widen :: Float -> Double
{-# NOINLINE widen #-}
widen = realToFrac

-- use GHC specific functions which work as expected [work for both -O0 and -O]
narrow' :: Double -> Float
{-# NOINLINE narrow' #-}
narrow' = double2Float

widen' :: Float -> Double
{-# NOINLINE widen' #-}
widen' = float2Double

doubleToBytes :: Double -> [Int]
doubleToBytes d
   = runST (do
        arr <- newArray_ ((0::Int),7)
        writeArray arr 0 d
        arr <- castDoubleToWord8Array arr
        i0 <- readArray arr 0
        i1 <- readArray arr 1
        i2 <- readArray arr 2
        i3 <- readArray arr 3
        i4 <- readArray arr 4
        i5 <- readArray arr 5
        i6 <- readArray arr 6
        i7 <- readArray arr 7
        return (map fromIntegral [i0,i1,i2,i3,i4,i5,i6,i7])
     )

castFloatToWord8Array :: STUArray s Int Float -> ST s (STUArray s Int Word8)
castFloatToWord8Array = castSTUArray

castDoubleToWord8Array :: STUArray s Int Double -> ST s (STUArray s Int Word8)
castDoubleToWord8Array = castSTUArray

dToStr :: Double -> String
dToStr d
  = let bs     = doubleToBytes d
        hex d' = case showHex d' "" of
                     []    -> error "dToStr: too few hex digits for float"
                     [x]   -> ['0',x]
                     [x,y] -> [x,y]
                     _     -> error "dToStr: too many hex digits for float"

        str  = map toUpper $ concat . fixEndian . (map hex) $ bs
    in  "0x" ++ str

fixEndian :: [a] -> [a]
#ifdef WORDS_BIGENDIAN
fixEndian = id
#else
fixEndian = reverse
#endif

