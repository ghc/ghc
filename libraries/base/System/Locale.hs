-----------------------------------------------------------------------------
-- 
-- Module      :  System.Locale
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- $Id: Locale.hs,v 1.1 2001/06/28 14:15:04 simonmar Exp $
--
-- Operations for defining locale-specific date and time formats.
--
-----------------------------------------------------------------------------

module System.Locale
    ( TimeLocale(..)
    , defaultTimeLocale
    
    , iso8601DateFormat
    , rfc822DateFormat
    )
where

import Prelude

data TimeLocale = TimeLocale {
        wDays  :: [(String, String)],   -- full and abbreviated week days
        months :: [(String, String)],   -- full and abbreviated months
        intervals :: [(String, String)],
        amPm   :: (String, String),     -- AM/PM symbols
        dateTimeFmt, dateFmt,           -- formatting strings
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


iso8601DateFormat :: Maybe String -> String
iso8601DateFormat timeFmt =
    "%Y-%m-%d" ++ case timeFmt of
             Nothing  -> "" -- normally, ISO-8601 just defines YYYY-MM-DD
             Just fmt -> ' ' : fmt -- but we can add a time spec


rfc822DateFormat :: String
rfc822DateFormat = "%a, %_d %b %Y %H:%M:%S %Z"
