{-# OPTIONS -fno-warn-unused-imports #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
#include "HsConfigure.h"
-- #hide
module Data.Time.Clock.Internal.NominalDiffTime
    (
    NominalDiffTime,
    nominalDay,
    ) where

import Data.Typeable
#if LANGUAGE_Rank2Types
import Data.Data
#endif
import Data.Fixed
import Data.Time.Calendar.Days
import Control.DeepSeq


-- | This is a length of time, as measured by UTC.
-- Conversion functions will treat it as seconds.
-- It has a precision of 10^-12 s.
-- It ignores leap-seconds, so it's not necessarily a fixed amount of clock time.
-- For instance, 23:00 UTC + 2 hours of NominalDiffTime = 01:00 UTC (+ 1 day),
-- regardless of whether a leap-second intervened.
newtype NominalDiffTime = MkNominalDiffTime Pico deriving (Eq,Ord
#if LANGUAGE_DeriveDataTypeable
#if LANGUAGE_Rank2Types
#if HAS_DataPico
    ,Data, Typeable
#endif
#endif
#endif
    )

-- necessary because H98 doesn't have "cunning newtype" derivation
instance NFData NominalDiffTime where -- FIXME: Data.Fixed had no NFData instances yet at time of writing
    rnf ndt = seq ndt ()

instance Enum NominalDiffTime where
    succ (MkNominalDiffTime a) = MkNominalDiffTime (succ a)
    pred (MkNominalDiffTime a) = MkNominalDiffTime (pred a)
    toEnum = MkNominalDiffTime . toEnum
    fromEnum (MkNominalDiffTime a) = fromEnum a
    enumFrom (MkNominalDiffTime a) = fmap MkNominalDiffTime (enumFrom a)
    enumFromThen (MkNominalDiffTime a) (MkNominalDiffTime b) = fmap MkNominalDiffTime (enumFromThen a b)
    enumFromTo (MkNominalDiffTime a) (MkNominalDiffTime b) = fmap MkNominalDiffTime (enumFromTo a b)
    enumFromThenTo (MkNominalDiffTime a) (MkNominalDiffTime b) (MkNominalDiffTime c) = fmap MkNominalDiffTime (enumFromThenTo a b c)

instance Show NominalDiffTime where
    show (MkNominalDiffTime t) = (showFixed True t) ++ "s"

-- necessary because H98 doesn't have "cunning newtype" derivation
instance Num NominalDiffTime where
    (MkNominalDiffTime a) + (MkNominalDiffTime b) = MkNominalDiffTime (a + b)
    (MkNominalDiffTime a) - (MkNominalDiffTime b) = MkNominalDiffTime (a - b)
    (MkNominalDiffTime a) * (MkNominalDiffTime b) = MkNominalDiffTime (a * b)
    negate (MkNominalDiffTime a) = MkNominalDiffTime (negate a)
    abs (MkNominalDiffTime a) = MkNominalDiffTime (abs a)
    signum (MkNominalDiffTime a) = MkNominalDiffTime (signum a)
    fromInteger i = MkNominalDiffTime (fromInteger i)

-- necessary because H98 doesn't have "cunning newtype" derivation
instance Real NominalDiffTime where
    toRational (MkNominalDiffTime a) = toRational a

-- necessary because H98 doesn't have "cunning newtype" derivation
instance Fractional NominalDiffTime where
    (MkNominalDiffTime a) / (MkNominalDiffTime b) = MkNominalDiffTime (a / b)
    recip (MkNominalDiffTime a) = MkNominalDiffTime (recip a)
    fromRational r = MkNominalDiffTime (fromRational r)

-- necessary because H98 doesn't have "cunning newtype" derivation
instance RealFrac NominalDiffTime where
    properFraction (MkNominalDiffTime a) = (i,MkNominalDiffTime f) where
        (i,f) = properFraction a
    truncate (MkNominalDiffTime a) = truncate a
    round (MkNominalDiffTime a) = round a
    ceiling (MkNominalDiffTime a) = ceiling a
    floor (MkNominalDiffTime a) = floor a

{-# RULES
"realToFrac/DiffTime->NominalDiffTime"   realToFrac = \ dt -> MkNominalDiffTime (realToFrac dt)
"realToFrac/NominalDiffTime->DiffTime"   realToFrac = \ (MkNominalDiffTime ps) -> realToFrac ps

"realToFrac/NominalDiffTime->Pico"       realToFrac = \ (MkNominalDiffTime ps) -> ps
"realToFrac/Pico->NominalDiffTime"       realToFrac = MkNominalDiffTime
  #-}

-- | One day in 'NominalDiffTime'.
nominalDay :: NominalDiffTime
nominalDay = 86400
