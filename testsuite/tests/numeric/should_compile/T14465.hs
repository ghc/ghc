{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE DataKinds             #-}

module M where

import Numeric.Natural
import GHC.Num.Natural

-- test Natural literals
one :: Natural
one = fromInteger 1

plusOne :: Natural -> Natural
plusOne n = n + 1

-- a built-in rule should convert this unfolding into a Natural literal in Core
ten :: Natural
ten = naturalFromWord 10

-- test basic constant folding for Natural
twoTimesTwo :: Natural
twoTimesTwo = 2 * 2

-- test the overflow warning
minusOne :: Natural
minusOne = -1
