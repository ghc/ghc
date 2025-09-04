module System.CPUTime.Utils
    ( -- * Integer conversions
      -- | These types have no 'Integral' instances in the Haskell report
      -- so we must do this ourselves.
      cClockToInteger
    , cTimeToInteger
    , csuSecondsToInteger
    ) where

import GHC.Internal.Foreign.C.Types
import GHC.Internal.Bignum.Integer (Integer)
import GHC.Internal.Real (fromIntegral)

cClockToInteger :: CClock -> Integer
cClockToInteger (CClock n) = fromIntegral n

cTimeToInteger :: CTime -> Integer
cTimeToInteger (CTime n) = fromIntegral n

csuSecondsToInteger :: CSUSeconds -> Integer
csuSecondsToInteger (CSUSeconds n) = fromIntegral n
