%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-99
%
\section[Time]{Haskell 1.4 Locale Library}


\begin{code}
module Locale(TimeLocale(..), defaultTimeLocale) where

import Prelude  -- so as to force recompilations when reqd.

data TimeLocale = TimeLocale {
        wDays  :: [(String, String)],   -- full and abbreviated week days
        months :: [(String, String)],   -- full and abbreviated months
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

        amPm = ("AM", "PM"),
        dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y",
        dateFmt = "%m/%d/%y",
        timeFmt = "%H:%M:%S",
        time12Fmt = "%I:%M:%S %p"
        }

\end{code}
