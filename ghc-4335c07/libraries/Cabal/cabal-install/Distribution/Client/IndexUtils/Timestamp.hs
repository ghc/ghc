{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.IndexUtils.Timestamp
-- Copyright   :  (c) 2016 Herbert Valerio Riedel
-- License     :  BSD3
--
-- Timestamp type used in package indexes

module Distribution.Client.IndexUtils.Timestamp
    ( Timestamp
    , nullTimestamp
    , epochTimeToTimestamp
    , timestampToUTCTime
    , utcTimeToTimestamp
    , maximumTimestamp

    , IndexState(..)
    ) where

import qualified Codec.Archive.Tar.Entry    as Tar
import           Control.DeepSeq
import           Control.Monad
import           Data.Char                  (isDigit)
import           Data.Int                   (Int64)
import           Data.Time                  (UTCTime (..), fromGregorianValid,
                                             makeTimeOfDayValid, showGregorian,
                                             timeOfDayToTime, timeToTimeOfDay)
import           Data.Time.Clock.POSIX      (posixSecondsToUTCTime,
                                             utcTimeToPOSIXSeconds)
import           Distribution.Compat.Binary
import qualified Distribution.Compat.ReadP  as ReadP
import           Distribution.Text
import qualified Text.PrettyPrint           as Disp
import           GHC.Generics (Generic)

-- | UNIX timestamp (expressed in seconds since unix epoch, i.e. 1970).
newtype Timestamp = TS Int64 -- Tar.EpochTime
                  deriving (Eq,Ord,Enum,NFData,Show)

epochTimeToTimestamp :: Tar.EpochTime -> Maybe Timestamp
epochTimeToTimestamp et
  | ts == nullTimestamp  = Nothing
  | otherwise            = Just ts
  where
    ts = TS et

timestampToUTCTime :: Timestamp -> Maybe UTCTime
timestampToUTCTime (TS t)
  | t == minBound  = Nothing
  | otherwise      = Just $ posixSecondsToUTCTime (fromIntegral t)

utcTimeToTimestamp :: UTCTime -> Maybe Timestamp
utcTimeToTimestamp utct
  | minTime <= t, t <= maxTime  = Just (TS (fromIntegral t))
  | otherwise                   = Nothing
  where
    maxTime = toInteger (maxBound :: Int64)
    minTime = toInteger (succ minBound :: Int64)
    t :: Integer
    t = round . utcTimeToPOSIXSeconds $ utct

-- | Compute the maximum 'Timestamp' value
--
-- Returns 'nullTimestamp' for the empty list.  Also note that
-- 'nullTimestamp' compares as smaller to all non-'nullTimestamp'
-- values.
maximumTimestamp :: [Timestamp] -> Timestamp
maximumTimestamp [] = nullTimestamp
maximumTimestamp xs@(_:_) = maximum xs

-- returns 'Nothing' if not representable as 'Timestamp'
posixSecondsToTimestamp :: Integer -> Maybe Timestamp
posixSecondsToTimestamp pt
  | minTs <= pt, pt <= maxTs  = Just (TS (fromInteger pt))
  | otherwise                 = Nothing
  where
    maxTs = toInteger (maxBound :: Int64)
    minTs = toInteger (succ minBound :: Int64)

-- | Pretty-prints 'Timestamp' in ISO8601/RFC3339 format
-- (e.g. @"2017-12-31T23:59:59Z"@)
--
-- Returns empty string for 'nullTimestamp' in order for
--
-- > null (display nullTimestamp) == True
--
-- to hold.
showTimestamp :: Timestamp -> String
showTimestamp ts = case timestampToUTCTime ts of
    Nothing          -> ""
    -- Note: we don't use 'formatTime' here to avoid incurring a
    -- dependency on 'old-locale' for older `time` libs
    Just UTCTime{..} -> showGregorian utctDay ++ ('T':showTOD utctDayTime) ++ "Z"
  where
    showTOD = show . timeToTimeOfDay

instance Binary Timestamp where
    put (TS t) = put t
    get = TS `fmap` get

instance Text Timestamp where
    disp = Disp.text . showTimestamp

    parse = parsePosix ReadP.+++ parseUTC
      where
        -- | Parses unix timestamps, e.g. @"\@1474626019"@
        parsePosix = do
            _ <- ReadP.char '@'
            t <- parseInteger
            maybe ReadP.pfail return $ posixSecondsToTimestamp t

        -- | Parses ISO8601/RFC3339-style UTC timestamps,
        -- e.g. @"2017-12-31T23:59:59Z"@
        --
        -- TODO: support numeric tz offsets; allow to leave off seconds
        parseUTC = do
            -- Note: we don't use 'Data.Time.Format.parseTime' here since
            -- we want more control over the accepted formats.

            ye <- parseYear
            _ <- ReadP.char '-'
            mo   <- parseTwoDigits
            _ <- ReadP.char '-'
            da   <- parseTwoDigits
            _ <- ReadP.char 'T'

            utctDay <- maybe ReadP.pfail return $
                       fromGregorianValid ye mo da

            ho   <- parseTwoDigits
            _ <- ReadP.char ':'
            mi   <- parseTwoDigits
            _ <- ReadP.char ':'
            se   <- parseTwoDigits
            _ <- ReadP.char 'Z'

            utctDayTime <- maybe ReadP.pfail (return . timeOfDayToTime) $
                           makeTimeOfDayValid ho mi (realToFrac (se::Int))

            maybe ReadP.pfail return $ utcTimeToTimestamp (UTCTime{..})

        parseTwoDigits = do
            d1 <- ReadP.satisfy isDigit
            d2 <- ReadP.satisfy isDigit
            return (read [d1,d2])

        -- A year must have at least 4 digits; e.g. "0097" is fine,
        -- while "97" is not c.f. RFC3339 which
        -- deprecates 2-digit years
        parseYear = do
            sign <- ReadP.option ' ' (ReadP.char '-')
            ds <- ReadP.munch1 isDigit
            when (length ds < 4) ReadP.pfail
            return (read (sign:ds))

        parseInteger = do
            sign <- ReadP.option ' ' (ReadP.char '-')
            ds <- ReadP.munch1 isDigit
            return (read (sign:ds) :: Integer)

-- | Special timestamp value to be used when 'timestamp' is
-- missing/unknown/invalid
nullTimestamp :: Timestamp
nullTimestamp = TS minBound

----------------------------------------------------------------------------
-- defined here for now to avoid import cycles

-- | Specification of the state of a specific repo package index
data IndexState = IndexStateHead -- ^ Use all available entries
                | IndexStateTime !Timestamp -- ^ Use all entries that existed at
                                            -- the specified time
                deriving (Eq,Generic,Show)

instance Binary IndexState
instance NFData IndexState

instance Text IndexState where
    disp IndexStateHead = Disp.text "HEAD"
    disp (IndexStateTime ts) = disp ts

    parse = parseHead ReadP.+++ parseTime
      where
        parseHead = do
            _ <- ReadP.string "HEAD"
            return IndexStateHead

        parseTime = IndexStateTime `fmap` parse
