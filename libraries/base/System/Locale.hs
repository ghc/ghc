-----------------------------------------------------------------------------
-- |
-- Module      :  System.Locale
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- This module provides the ability to adapt to local conventions.
-- At present, it supports only time and date information as used by
-- 'System.Time.calendarTimeToString' from the "System.Time" module.
--
-----------------------------------------------------------------------------

module System.Locale (

    TimeLocale(..)

    , defaultTimeLocale
    
    , iso8601DateFormat
    , rfc822DateFormat
    )
where

import Prelude

data TimeLocale = TimeLocale {
	-- |full and abbreviated week days
        wDays  :: [(String, String)],
	-- |full and abbreviated months
        months :: [(String, String)],
        intervals :: [(String, String)],
	-- |AM\/PM symbols
        amPm   :: (String, String),
	-- |formatting strings
        dateTimeFmt, dateFmt,
        timeFmt, time12Fmt :: String     
        } deriving (Eq, Ord, Show)

defaultTimeLocale :: TimeLocale 
defaultTimeLocale =  TimeLocale { 
        wDays  = [("Sunday",   "Sun"),  ("Monday",    "Mon"),   
                  ("Tuesday",  "Tue"),  ("Wednesday", "Wed"), 
                  ("Thursday", "Thu"),  ("Friday",    "Fri"), 
                  ("Saturday", "Sat")],

        months = [("January",   "Jan"), ("February",  "Feb"),
                  ("March",     "Mar"), ("April",     "Apr"),
                  ("May",       "May"), ("June",      "Jun"),
                  ("July",      "Jul"), ("August",    "Aug"),
                  ("September", "Sep"), ("October",   "Oct"),
                  ("November",  "Nov"), ("December",  "Dec")],

        intervals = [ ("year","years")
                    , ("month", "months")
                    , ("day","days")
                    , ("hour","hours")
                    , ("min","mins")
                    , ("sec","secs")
                    , ("usec","usecs")
                    ],

        amPm = ("AM", "PM"),
        dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y",
        dateFmt = "%m/%d/%y",
        timeFmt = "%H:%M:%S",
        time12Fmt = "%I:%M:%S %p"
        }


-- |Normally, ISO-8601 just defines YYYY-MM-DD
-- but we can add a time spec.

iso8601DateFormat :: Maybe String -> String
iso8601DateFormat timeFmt =
    "%Y-%m-%d" ++ case timeFmt of
             Nothing  -> "" 
             Just fmt -> ' ' : fmt


rfc822DateFormat :: String
rfc822DateFormat = "%a, %_d %b %Y %H:%M:%S %Z"
