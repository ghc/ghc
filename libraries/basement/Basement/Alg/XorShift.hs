-- |
-- Module      : Foundation.Random.XorShift
-- License     : BSD-style
--
-- XorShift variant: Xoroshiro128+
-- <https://en.wikipedia.org/wiki/Xoroshiro128%2B>
--
-- Xoroshiro128+ is a PRNG that uses a shift/rotate-based linear transformation.
-- This is lar
--
-- C implementation at:
-- <http://xoroshiro.di.unimi.it/xoroshiro128plus.c>
--
module Basement.Alg.XorShift
    ( State(..)
    , next
    , nextDouble
    , jump
    ) where

import           Data.Word
import           Data.Bits
import           Basement.Compat.Base
import           Basement.Floating (wordToDouble)
import           Basement.Numerical.Additive
import           Basement.Numerical.Subtractive

-- | State of Xoroshiro128 plus
data State = State {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64

-- | Given a state, call the function 'f' with the generated Word64 and the next State
next :: State -> (Word64 -> State -> a) -> a
next (State s0 s1prev) f = f ran stNext
  where
    !stNext = State s0' s1'
    !ran    = s0 + s1prev
    !s1     = s0 `xor` s1prev
    s0'     = (s0 `rotateL` 55) `xor` s1 `xor` (s1 `unsafeShiftL` 14)
    s1'     = (s1 `rotateL` 36)

-- | Same as 'next' but give a random value of type Double in the range of [0.0 .. 1.0]
nextDouble :: State -> (Double -> State -> a) -> a
nextDouble st f = next st $ \w -> f (toDouble w)
  where
    -- generate a number in the interval [1..2[ by bit manipulation.
    -- this generate double with a ~2^52
    toDouble w = wordToDouble (upperMask .|. (w .&. lowerMask)) - 1.0
      where
        upperMask = 0x3FF0000000000000
        lowerMask = 0x000FFFFFFFFFFFFF

-- | Jump the state by 2^64 calls of next
jump :: State -> State
jump (State s0 s1) = withK 0xd86b048b86aa9922
                   $ withK 0xbeac0467eba5facb
                   $ (State 0 0)
  where
    withK :: Word64 -> State -> State
    withK !k = loop 0
      where
        loop !i st@(State c0 c1)
            | i == 64     = st
            | testBit k i = loop (i+1) (State (c0 `xor` s0) (c1 `xor` s1))
            | otherwise   = st
