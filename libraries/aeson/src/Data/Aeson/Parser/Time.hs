{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aeson.Parser.Time
    (
      run
    , day
    , month
    , quarter
    , localTime
    , timeOfDay
    , timeZone
    , utcTime
    , zonedTime
    ) where

import Prelude.Compat

import Data.Attoparsec.Text (Parser)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Time.Calendar.Quarter.Compat (Quarter)
import Data.Time.Calendar.Month.Compat (Month)
import Data.Time.Clock (UTCTime(..))
import qualified Data.Aeson.Types.Internal as Aeson
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Time as T
import qualified Data.Time.LocalTime as Local

-- | Run an attoparsec parser as an aeson parser.
run :: Parser a -> Text -> Aeson.Parser a
run p t = case A.parseOnly (p <* A.endOfInput) t of
            Left err -> fail $ "could not parse date: " ++ err
            Right r  -> return r

-- | Parse a date of the form @[+,-]YYYY-MM-DD@.
day :: Parser Day
day = T.day
{-# INLINE day #-}

-- | Parse a date of the form @[+,-]YYYY-MM@.
month :: Parser Month
month = T.month
{-# INLINE month #-}

-- | Parse a date of the form @[+,-]YYYY-QN@.
quarter :: Parser Quarter
quarter = T.quarter
{-# INLINE quarter #-}

-- | Parse a time of the form @HH:MM[:SS[.SSS]]@.
timeOfDay :: Parser Local.TimeOfDay
timeOfDay = T.timeOfDay
{-# INLINE timeOfDay #-}

-- | Parse a quarter of the form @[+,-]YYYY-QN@.

-- | Parse a time zone, and return 'Nothing' if the offset from UTC is
-- zero. (This makes some speedups possible.)
timeZone :: Parser (Maybe Local.TimeZone)
timeZone = T.timeZone
{-# INLINE timeZone #-}

-- | Parse a date and time, of the form @YYYY-MM-DD HH:MM[:SS[.SSS]]@.
-- The space may be replaced with a @T@.  The number of seconds is optional
-- and may be followed by a fractional component.
localTime :: Parser Local.LocalTime
localTime = T.localTime
{-# INLINE localTime #-}

-- | Behaves as 'zonedTime', but converts any time zone offset into a
-- UTC time.
utcTime :: Parser UTCTime
utcTime = T.utcTime
{-# INLINE utcTime #-}

-- | Parse a date with time zone info. Acceptable formats:
--
-- @YYYY-MM-DD HH:MM Z@
-- @YYYY-MM-DD HH:MM:SS Z@
-- @YYYY-MM-DD HH:MM:SS.SSS Z@
--
-- The first space may instead be a @T@, and the second space is
-- optional.  The @Z@ represents UTC.  The @Z@ may be replaced with a
-- time zone offset of the form @+0000@ or @-08:00@, where the first
-- two digits are hours, the @:@ is optional and the second two digits
-- (also optional) are minutes.
zonedTime :: Parser Local.ZonedTime
zonedTime = T.zonedTime
{-# INLINE zonedTime #-}
