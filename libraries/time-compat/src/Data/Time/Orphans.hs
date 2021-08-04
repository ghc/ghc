{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Time.Orphans () where

import Data.Orphans ()

import Control.DeepSeq (NFData (..))
import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.TAI
import Data.Time.Format
import Data.Hashable (Hashable (..))

#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (TimeLocale (..))
#else
import System.Locale (TimeLocale (..))
#endif

#if MIN_VERSION_time(1,8,0)
import Data.Time.Clock.System
#endif

#if !MIN_VERSION_time(1,11,0)
import Data.Fixed (Pico)
import Text.Read (Read (..))
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec
#endif

#if MIN_VERSION_time(1,11,0)
import Data.Ix (Ix (..))
import Data.Time.Calendar.Month
import Data.Time.Calendar.Quarter
#endif

#if !MIN_VERSION_time(1,6,0)
instance ParseTime UniversalTime where
    -- substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    -- parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l xs = localTimeToUT1 0 (buildTime l xs)

instance FormatTime UniversalTime where
    formatCharacter c = fmap (\f tl fo t -> f tl fo (ut1ToLocalTime 0 t)) (formatCharacter c)

instance Show UniversalTime where
    show t = show (ut1ToLocalTime 0 t)

instance Read UniversalTime where
    readsPrec n s = [ (localTimeToUT1 0 t, r) | (t,r) <- readsPrec n s ]
#endif


#if MIN_VERSION_time(1,9,0) && !MIN_VERSION_time(1,11,0)
deriving instance Ord DayOfWeek
#endif

#if MIN_VERSION_time(1,9,0) && !MIN_VERSION_time(1,10,0)
#if __GLASGOW_HASKELL__ <710
deriving instance Typeable DayOfWeek
#endif
deriving instance Data DayOfWeek
#endif

#if MIN_VERSION_time(1,8,0) && !MIN_VERSION_time(1,10,0)
#if __GLASGOW_HASKELL__ <710
deriving instance Typeable SystemTime
#endif

deriving instance Data SystemTime
#endif

#if MIN_VERSION_time(1,9,0) && !MIN_VERSION_time(1,11,1)
instance NFData DayOfWeek where
    rnf !_ = ()

instance NFData CalendarDiffTime where
    rnf (CalendarDiffTime x y) = rnf x `seq` rnf y

instance NFData CalendarDiffDays where
    rnf (CalendarDiffDays x y) = rnf x `seq` rnf y
#endif

#if !MIN_VERSION_time(1,11,0)

instance Read DiffTime where
    readPrec = do
        t <- readPrec :: ReadPrec Pico
        _ <- lift $ char 's'
        return $ realToFrac t

instance Read NominalDiffTime where
    readPrec = do
        t <- readPrec :: ReadPrec Pico
        _ <- lift $ char 's'
        return $ realToFrac t

#endif

#if MIN_VERSION_time(1,11,0) && !MIN_VERSION_time(1,11,1)
instance NFData Month where
    rnf (MkMonth m) = rnf m

instance Enum Month where
    succ (MkMonth a) = MkMonth (succ a)
    pred (MkMonth a) = MkMonth (pred a)
    toEnum = MkMonth . toEnum
    fromEnum (MkMonth a) = fromEnum a
    enumFrom (MkMonth a) = fmap MkMonth (enumFrom a)
    enumFromThen (MkMonth a) (MkMonth b) = fmap MkMonth (enumFromThen a b)
    enumFromTo (MkMonth a) (MkMonth b) = fmap MkMonth (enumFromTo a b)
    enumFromThenTo (MkMonth a) (MkMonth b) (MkMonth c) =
        fmap MkMonth (enumFromThenTo a b c)

instance Ix Month where
    range (MkMonth a, MkMonth b) = fmap MkMonth (range (a, b))
    index (MkMonth a, MkMonth b) (MkMonth c) = index (a, b) c
    inRange (MkMonth a, MkMonth b) (MkMonth c) = inRange (a, b) c
    rangeSize (MkMonth a, MkMonth b) = rangeSize (a, b)

instance NFData QuarterOfYear where
    rnf Q1 = ()
    rnf Q2 = ()
    rnf Q3 = ()
    rnf Q4 = ()

instance NFData Quarter where
    rnf (MkQuarter m) = rnf m

instance Enum Quarter where
    succ (MkQuarter a) = MkQuarter (succ a)
    pred (MkQuarter a) = MkQuarter (pred a)
    toEnum = MkQuarter . toEnum
    fromEnum (MkQuarter a) = fromEnum a
    enumFrom (MkQuarter a) = fmap MkQuarter (enumFrom a)
    enumFromThen (MkQuarter a) (MkQuarter b) = fmap MkQuarter (enumFromThen a b)
    enumFromTo (MkQuarter a) (MkQuarter b) = fmap MkQuarter (enumFromTo a b)
    enumFromThenTo (MkQuarter a) (MkQuarter b) (MkQuarter c) =
        fmap MkQuarter (enumFromThenTo a b c)

instance Ix Quarter where
    range (MkQuarter a, MkQuarter b) = fmap MkQuarter (range (a, b))
    index (MkQuarter a, MkQuarter b) (MkQuarter c) = index (a, b) c
    inRange (MkQuarter a, MkQuarter b) (MkQuarter c) = inRange (a, b) c
    rangeSize (MkQuarter a, MkQuarter b) = rangeSize (a, b)
#endif

-------------------------------------------------------------------------------
-- Hashable
-------------------------------------------------------------------------------

instance Hashable UniversalTime where
    hashWithSalt salt = hashWithSalt salt . getModJulianDate

instance Hashable DiffTime where
    hashWithSalt salt = hashWithSalt salt . toRational

instance Hashable UTCTime where
    hashWithSalt salt (UTCTime d dt) =
        salt `hashWithSalt` d `hashWithSalt` dt

instance Hashable NominalDiffTime where
    hashWithSalt salt = hashWithSalt salt . toRational

instance Hashable Day where
    hashWithSalt salt (ModifiedJulianDay d) = hashWithSalt salt d

instance Hashable TimeZone where
    hashWithSalt salt (TimeZone m s n) =
        salt `hashWithSalt` m `hashWithSalt` s `hashWithSalt` n

instance Hashable TimeOfDay where
    hashWithSalt salt (TimeOfDay h m s) =
        salt `hashWithSalt` h `hashWithSalt` m `hashWithSalt` s

instance Hashable LocalTime where
    hashWithSalt salt (LocalTime d tod) =
        salt `hashWithSalt` d `hashWithSalt` tod

instance Hashable TimeLocale where
    hashWithSalt salt (TimeLocale a b c d e f g h) =
      salt `hashWithSalt` a
           `hashWithSalt` b
           `hashWithSalt` c
           `hashWithSalt` d
           `hashWithSalt` e
           `hashWithSalt` f
           `hashWithSalt` g
           `hashWithSalt` h

#if MIN_VERSION_time(1,9,0)
instance Hashable DayOfWeek where
    hashWithSalt salt = hashWithSalt salt . fromEnum
#endif

#if MIN_VERSION_time(1,11,0)
instance Hashable Month where
    hashWithSalt salt (MkMonth x) = hashWithSalt salt x

instance Hashable Quarter where
    hashWithSalt salt (MkQuarter x) = hashWithSalt salt x

instance Hashable QuarterOfYear where
    hashWithSalt salt = hashWithSalt salt . fromEnum
#endif
