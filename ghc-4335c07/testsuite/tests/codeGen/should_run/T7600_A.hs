-- !!! Bug # 7600.
-- See file T7600 for main description.
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

doubleToWord64 :: Double -> Word64
doubleToWord64 d
   = runST (do
        arr <- newArray_ ((0::Int),0)
        writeArray arr 0 d
        arr <- castDoubleToWord64Array arr
        readArray arr 0
     )

castFloatToWord64Array :: STUArray s Int Float -> ST s (STUArray s Int Word64)
castFloatToWord64Array = castSTUArray

castDoubleToWord64Array :: STUArray s Int Double -> ST s (STUArray s Int Word64)
castDoubleToWord64Array = castSTUArray

dToStr :: Double -> String
dToStr d
  = let bs     = doubleToWord64 d
        hex d' = showHex d' ""

        str  = map toUpper $ hex bs
    in  "0x" ++ str

