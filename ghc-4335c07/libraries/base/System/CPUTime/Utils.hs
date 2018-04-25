module System.CPUTime.Utils
    ( -- * Integer conversions
      -- | These types have no 'Integral' instances in the Haskell report
      -- so we must do this ourselves.
      cClockToInteger
    , cTimeToInteger
    , csuSecondsToInteger
    ) where

import Foreign.C.Types

cClockToInteger :: CClock -> Integer
cClockToInteger (CClock n) = fromIntegral n

cTimeToInteger :: CTime -> Integer
cTimeToInteger (CTime n) = fromIntegral n

csuSecondsToInteger :: CSUSeconds -> Integer
csuSecondsToInteger (CSUSeconds n) = fromIntegral n
