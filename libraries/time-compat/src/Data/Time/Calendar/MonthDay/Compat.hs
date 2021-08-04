{-# LANGUAGE CPP #-}
module Data.Time.Calendar.MonthDay.Compat (
    MonthOfYear, DayOfMonth, DayOfYear,
    monthAndDayToDayOfYear,
    monthAndDayToDayOfYearValid,
    dayOfYearToMonthAndDay,
    monthLength,
) where

import Data.Time.Orphans ()

import Data.Time.Calendar.MonthDay

#if !MIN_VERSION_time(1,11,0)
import Data.Time.Calendar.Types
#endif
