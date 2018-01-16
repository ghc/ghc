module Data.Time.Calendar.Easter
    (
    sundayAfter,
    orthodoxPaschalMoon,orthodoxEaster,
    gregorianPaschalMoon,gregorianEaster
    ) where

-- formulae from Reingold & Dershowitz, _Calendrical Calculations_, ch. 8.

import Data.Time.Calendar
import Data.Time.Calendar.Julian

-- | The next Sunday strictly after a given day.
sundayAfter :: Day -> Day
sundayAfter day = addDays (7 - (mod (toModifiedJulianDay day + 3) 7)) day

-- | Given a year, find the Paschal full moon according to Orthodox Christian tradition
orthodoxPaschalMoon :: Integer -> Day
orthodoxPaschalMoon year  = addDays (- shiftedEpact) (fromJulian jyear 4 19) where
    shiftedEpact = mod (14 + 11 * (mod year 19)) 30
    jyear = if year > 0 then year else year - 1

-- | Given a year, find Easter according to Orthodox Christian tradition
orthodoxEaster :: Integer -> Day
orthodoxEaster = sundayAfter . orthodoxPaschalMoon

-- | Given a year, find the Paschal full moon according to the Gregorian method
gregorianPaschalMoon :: Integer -> Day
gregorianPaschalMoon year  = addDays (- adjustedEpact) (fromGregorian year 4 19) where
    century = (div year 100) + 1
    shiftedEpact = mod (14 + 11 * (mod year 19) - (div (3 * century) 4) + (div (5 + 8 * century) 25)) 30
    adjustedEpact = if shiftedEpact == 0 || ((shiftedEpact == 1) && (mod year 19 < 10)) then shiftedEpact + 1 else shiftedEpact

-- | Given a year, find Easter according to the Gregorian method
gregorianEaster :: Integer -> Day
gregorianEaster = sundayAfter . gregorianPaschalMoon
