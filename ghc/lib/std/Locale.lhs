%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-99
%
\section[Time]{Haskell 1.4 Locale Library}


\begin{code}
module Locale
    ( TimeLocale(..)
    , defaultTimeLocale
    
    , iso8601DateFormat
    , rfc822DateFormat
    )
where

import Prelude  -- so as to force recompilations when reqd.

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
\end{code}
