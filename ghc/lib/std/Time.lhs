%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-99
%
\section[Time]{Haskell 1.4 Time of Day Library}

The {\em Time} library provides standard functionality for
clock times, including timezone information (i.e, the functionality of
"time.h",  adapted to the Haskell environment), It follows RFC 1129 in
its use of Coordinated Universal Time (UTC).

\begin{code}
{-# OPTIONS -#include "cbits/timezone.h" -#include "cbits/stgio.h"  #-}
module Time 
     (
        Month(..)
     ,  Day(..)

     ,  ClockTime(..) -- non-standard, lib. report gives this as abstract
     ,	getClockTime

     ,  TimeDiff(..)
     ,  diffClockTimes
     ,  addToClockTime

     ,  timeDiffToString  -- non-standard
     ,  formatTimeDiff    -- non-standard

     ,  CalendarTime(..)
     ,	toCalendarTime
     ,  toUTCTime
     ,  toClockTime
     ,  calendarTimeToString
     ,  formatCalendarTime

     ) where

#ifdef __HUGS__
import PreludeBuiltin
#else
import PrelBase
import PrelShow
import PrelIOBase
import PrelHandle
import PrelArr
import PrelST
import PrelAddr
import PrelPack 	( unpackCString )
#endif

import Ix
import Char     	( intToDigit )
import Locale

\end{code}

One way to partition and give name to chunks of a year and a week:

\begin{code}
data Month
 = January   | February | March    | April
 | May       | June     | July     | August
 | September | October  | November | December
 deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

data Day 
 = Sunday   | Monday | Tuesday | Wednesday
 | Thursday | Friday | Saturday
 deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

\end{code}

@ClockTime@ is an abstract type, used for the internal clock time.
Clock times may be compared, converted to strings, or converted to an
external calendar time @CalendarTime@.

\begin{code}
#ifdef __HUGS__
-- I believe Int64 is more than big enough.
-- In fact, I think one of Int32 or Word32 would do. - ADR
data ClockTime = TOD Int64 Int64 deriving (Eq, Ord)
#else
data ClockTime = TOD Integer Integer deriving (Eq, Ord)
#endif
\end{code}

When a @ClockTime@ is shown, it is converted to a string of the form
@"Mon Nov 28 21:45:41 GMT 1994"@.

For now, we are restricted to roughly:
Fri Dec 13 20:45:52 1901 through Tue Jan 19 03:14:07 2038, because
we use the C library routines based on 32 bit integers.

\begin{code}
#ifdef __HUGS__
#warning Show ClockTime is bogus
instance Show ClockTime
#else
instance Show ClockTime where
    showsPrec p (TOD (S# i) _nsec) = 
      case int2Integer# i of (# s, d #) -> showsPrec p (TOD (J# s d) _nsec)
    showsPrec _ (TOD (J# s# d#) _nsec) = 
      showString $ unsafePerformIO $ do
	    buf <- allocChars 38 -- exactly enough for error message
	    str <- _ccall_ showTime (I# s#) d# buf
	    return (unpackCString str)

    showList = showList__ (showsPrec 0)
#endif
\end{code}


@CalendarTime@ is a user-readable and manipulable
representation of the internal $ClockTime$ type.  The
numeric fields have the following ranges.

\begin{verbatim}
Value         Range             Comments
-----         -----             --------

year    -maxInt .. maxInt       [Pre-Gregorian dates are inaccurate]
mon           0 .. 11           [Jan = 0, Dec = 11]
day           1 .. 31
hour          0 .. 23
min           0 .. 59
sec           0 .. 61           [Allows for two leap seconds]
picosec       0 .. (10^12)-1    [This could be over-precise?]
wday          0 .. 6            [Sunday = 0, Saturday = 6]
yday          0 .. 365          [364 in non-Leap years]
tz       -43200 .. 43200        [Variation from UTC in seconds]
\end{verbatim}

The {\em tzname} field is the name of the time zone.  The {\em isdst}
field indicates whether Daylight Savings Time would be in effect.

\begin{code}
data CalendarTime 
 = CalendarTime  {
     ctYear    :: Int,
     ctMonth   :: Int,
     ctDay     :: Int,
     ctHour    :: Int,
     ctMin     :: Int,
     ctSec     :: Int,
#ifdef __HUGS__
     ctPicosec :: Int64,
#else
     ctPicosec :: Integer,
#endif
     ctWDay    :: Day,
     ctYDay    :: Int,
     ctTZName  :: String,
     ctTZ      :: Int,
     ctIsDST   :: Bool
 }
 deriving (Eq,Ord,Read,Show)

\end{code}

The @TimeDiff@ type records the difference between two clock times in
a user-readable way.

\begin{code}
data TimeDiff
 = TimeDiff {
     tdYear    :: Int,
     tdMonth   :: Int,
     tdDay     :: Int,
     tdHour    :: Int,
     tdMin     :: Int,
     tdSec     :: Int,
#ifdef __HUGS__
     tdPicosec :: Int64   -- not standard
#else
     tdPicosec :: Integer -- not standard
#endif
   }
   deriving (Eq,Ord,Read,Show)
\end{code}

@getClockTime@ returns the current time in its internal representation.

\begin{code}
#ifdef __HUGS__
getClockTime :: IO ClockTime
getClockTime = do
    i1 <- malloc1
    i2 <- malloc1
    rc <- prim_getClockTime i1 i2
    if rc == 0 
	then do
	    sec  <- cvtUnsigned i1
	    nsec <- cvtUnsigned i2
	    return (TOD sec (nsec * 1000))
    	else
	    constructErrorAndFail "getClockTime"
  where
    malloc1 = primNewByteArray sizeof_int64
    cvtUnsigned arr = primReadInt64Array arr 0
#else
getClockTime :: IO ClockTime
getClockTime = do
    i1 <- malloc1
    i2 <- malloc1
    rc <- _ccall_ getClockTime i1 i2
    if rc == (0 ::Int)
	then do
	    sec  <- cvtUnsigned i1
	    nsec <- cvtUnsigned i2
	    return (TOD sec (nsec * 1000))
    	else
	    constructErrorAndFail "getClockTime"
  where
    malloc1 = IO $ \ s# ->
	case newIntArray# 1# s# of 
          (# s2#, barr# #) -> 
		(# s2#, MutableByteArray bottom barr# #)

    --  The C routine fills in an unsigned word.  We don't have 
    --	`unsigned2Integer#,' so we freeze the data bits and use them 
    --	for an MP_INT structure.  Note that zero is still handled specially,
    --	although (J# 1# (ptr to 0#)) is probably acceptable to gmp.

    cvtUnsigned (MutableByteArray _ arr#) = IO $ \ s# ->
	case readIntArray# arr# 0# s# of 
	  (# s2#, r# #) ->
            if r# ==# 0# 
		then (# s2#, 0 #)
            	else case unsafeFreezeByteArray# arr# s2# of
                        (# s3#, frozen# #) -> 
				(# s3#, J# 1# frozen# #)
#endif
\end{code}

@addToClockTime@ {\em d} {\em t} adds a time difference {\em d} and a
clock time {\em t} to yield a new clock time.  The difference {\em d}
may be either positive or negative.  @[diffClockTimes@ {\em t1} {\em
t2} returns the difference between two clock times {\em t1} and {\em
t2} as a @TimeDiff@.


\begin{code}
#ifdef __HUGS__
addToClockTime  :: TimeDiff  -> ClockTime -> ClockTime
addToClockTime (TimeDiff year mon day hour min sec psec) 
	       (TOD c_sec c_psec) = unsafePerformIO $ do
    res <- allocWords sizeof_int64
    rc <- prim_toClockSec year mon day hour min sec 0 res 
    if rc /= (0::Int)
     then do
            diff_sec <- primReadInt64Array res 0
	    let diff_psec = psec
            return (TOD (c_sec + diff_sec) (c_psec + diff_psec))
     else
          error "Time.addToClockTime: can't perform conversion of TimeDiff"
#else
addToClockTime  :: TimeDiff  -> ClockTime -> ClockTime
addToClockTime (TimeDiff year mon day hour min sec psec) 
	       (TOD c_sec c_psec) = unsafePerformIO $ do
    res <- allocWords (``sizeof(time_t)'')
    ptr <- _ccall_ toClockSec year mon day hour min sec (0::Int) res 
    let (A# ptr#) = ptr
    if ptr /= nullAddr
     then let
	    diff_sec  = (int2Integer (indexIntOffAddr# ptr# 0#))
	    diff_psec = psec
       	  in
          return (TOD (c_sec + diff_sec) (c_psec + diff_psec))
     else
          error "Time.addToClockTime: can't perform conversion of TimeDiff"
#endif

diffClockTimes  :: ClockTime -> ClockTime -> TimeDiff
diffClockTimes tod_a tod_b =
  let
   CalendarTime year_a mon_a day_a hour_a min_a sec_a psec_a _ _ _ _ _ = toUTCTime tod_a
   CalendarTime year_b mon_b day_b hour_b min_b sec_b psec_b _ _ _ _ _ = toUTCTime tod_b
  in
  TimeDiff (year_a - year_b) 
	   (mon_a  - mon_b) 
	   (day_a  - day_b)
	   (hour_a - hour_b)
	   (min_a  - min_b)
	   (sec_a  - sec_b)
	   (psec_a - psec_b)
\end{code}

@toCalendarTime@ {\em t} converts {\em t} to a local time, modified by
the current timezone and daylight savings time settings.  @toUTCTime@
{\em t} converts {\em t} into UTC time.  @toClockTime@ {\em l}
converts {\em l} into the corresponding internal @ClockTime@.  The
{\em wday}, {\em yday}, {\em tzname}, and {\em isdst} fields are
ignored.

\begin{code}
#ifdef __HUGS__
toCalendarTime :: ClockTime -> IO CalendarTime
toCalendarTime (TOD sec psec) = do
    res    <- allocWords sizeof_int64
    zoneNm <- allocChars 32
    prim_SETZONE res zoneNm
    rc <- prim_toLocalTime sec res
    if rc /= 0
     then constructErrorAndFail "Time.toCalendarTime: out of range"
     else do
       sec   <-  get_tm_sec   res
       min   <-  get_tm_min   res
       hour  <-  get_tm_hour  res
       mday  <-  get_tm_mday  res
       mon   <-  get_tm_mon   res
       year  <-  get_tm_year  res
       wday  <-  get_tm_wday  res
       yday  <-  get_tm_yday  res
       isdst <-  get_tm_isdst res
       zone  <-  prim_ZONE    res
       tz    <-  prim_GMTOFF  res
       tzname <- primUnpackCString zone
       return (CalendarTime (1900+year) mon mday hour min sec psec 
            		    (toEnum wday) yday tzname tz (isdst /= 0))

toUTCTime :: ClockTime -> CalendarTime
toUTCTime  (TOD sec psec) = unsafePerformIO $ do
       res    <- allocWords sizeof_int64
       zoneNm <- allocChars 32
       prim_SETZONE res zoneNm
       rc <- prim_toUTCTime sec res
       if rc /= 0
	then error "Time.toUTCTime: out of range"
        else do
	    sec   <- get_tm_sec  res
	    min   <- get_tm_min  res
	    hour  <- get_tm_hour res
	    mday  <- get_tm_mday res
	    mon   <- get_tm_mon  res
	    year  <- get_tm_year res
	    wday  <- get_tm_wday res
	    yday  <- get_tm_yday res
            return (CalendarTime (1900+year) mon mday hour min sec psec 
            		  (toEnum wday) yday "UTC" 0 False)

toClockTime :: CalendarTime -> ClockTime
toClockTime (CalendarTime year mon mday hour min sec psec wday yday tzname tz isdst) =
    if psec < 0 || psec > 999999999999 then
        error "Time.toClockTime: picoseconds out of range"
    else if tz < -43200 || tz > 43200 then
        error "Time.toClockTime: timezone offset out of range"
    else
        unsafePerformIO ( do
	    res <- allocWords sizeof_int64
	    rc <- prim_toClockSec year mon mday hour min sec isDst res
            if rc /= (0::Int)
             then do
               tm <- primReadInt64Array res 0
               return (TOD tm psec)
	     else error "Time.toClockTime: can't perform conversion"
        )
    where
     isDst = if isdst then (1::Int) else 0
#else
toCalendarTime :: ClockTime -> IO CalendarTime
toCalendarTime (TOD (S# i) psec) 
  = case int2Integer# i of (# s, d #) -> toCalendarTime (TOD (J# s d) psec)
toCalendarTime (TOD (J# s# d#) psec) = do
    res    <- allocWords (``sizeof(struct tm)''::Int)
    zoneNm <- allocChars 32
    _casm_ ``SETZONE((struct tm *)%0,(char *)%1); '' res zoneNm
    tm     <- _ccall_ toLocalTime (I# s#) d# res
    if tm == nullAddr
     then constructErrorAndFail "Time.toCalendarTime: out of range"
     else do
       sec   <-  _casm_ ``%r = ((struct tm *)%0)->tm_sec;'' tm
       min   <-  _casm_ ``%r = ((struct tm *)%0)->tm_min;'' tm
       hour  <-  _casm_ ``%r = ((struct tm *)%0)->tm_hour;'' tm
       mday  <-  _casm_ ``%r = ((struct tm *)%0)->tm_mday;'' tm
       mon   <-  _casm_ ``%r = ((struct tm *)%0)->tm_mon;'' tm
       year  <-  _casm_ ``%r = ((struct tm *)%0)->tm_year;'' tm
       wday  <-  _casm_ ``%r = ((struct tm *)%0)->tm_wday;'' tm
       yday  <-  _casm_ ``%r = ((struct tm *)%0)->tm_yday;'' tm
       isdst <-  _casm_ ``%r = ((struct tm *)%0)->tm_isdst;'' tm
       zone  <-  _ccall_ get_ZONE tm
       tz    <-  _ccall_ GMTOFF tm
       let tzname = unpackCString zone
       return (CalendarTime (1900+year) mon mday hour min sec psec 
            		    (toEnum wday) yday tzname tz (isdst /= (0::Int)))

toUTCTime :: ClockTime -> CalendarTime
toUTCTime (TOD (S# i) psec) 
  = case int2Integer# i of (# s, d #) -> toUTCTime (TOD (J# s d) psec)
toUTCTime  (TOD (J# s# d#) psec) = unsafePerformIO $ do
       res    <- allocWords (``sizeof(struct tm)''::Int)
       zoneNm <- allocChars 32
       _casm_ ``SETZONE((struct tm *)%0,(char *)%1); '' res zoneNm
       tm     <-  _ccall_ toUTCTime (I# s#) d# res
       if tm == nullAddr
	then error "Time.toUTCTime: out of range"
        else do
	    sec   <- _casm_ ``%r = ((struct tm *)%0)->tm_sec;'' tm
	    min   <- _casm_ ``%r = ((struct tm *)%0)->tm_min;'' tm
	    hour  <- _casm_ ``%r = ((struct tm *)%0)->tm_hour;'' tm
	    mday  <- _casm_ ``%r = ((struct tm *)%0)->tm_mday;'' tm
	    mon   <- _casm_ ``%r = ((struct tm *)%0)->tm_mon;'' tm
	    year  <- _casm_ ``%r = ((struct tm *)%0)->tm_year;'' tm
	    wday  <- _casm_ ``%r = ((struct tm *)%0)->tm_wday;'' tm
	    yday  <- _casm_ ``%r = ((struct tm *)%0)->tm_yday;'' tm
            return (CalendarTime (1900+year) mon mday hour min sec psec 
            		  (toEnum wday) yday "UTC" 0 False)

toClockTime :: CalendarTime -> ClockTime
toClockTime (CalendarTime year mon mday hour min sec psec _wday _yday _tzname tz isdst) =
    if psec < 0 || psec > 999999999999 then
        error "Time.toClockTime: picoseconds out of range"
    else if tz < -43200 || tz > 43200 then
        error "Time.toClockTime: timezone offset out of range"
    else
        unsafePerformIO ( do
	    res <- allocWords (``sizeof(time_t)'')
	    ptr <- _ccall_ toClockSec year mon mday hour min sec isDst res
            let (A# ptr#) = ptr
            if ptr /= nullAddr
             then return (TOD (int2Integer (indexIntOffAddr# ptr# 0#)) psec)
	     else error "Time.toClockTime: can't perform conversion"
        )
    where
     isDst = if isdst then (1::Int) else 0
#endif

bottom :: (Int,Int)
bottom = error "Time.bottom"


-- (copied from PosixUtil, for now)
-- Allocate a mutable array of characters with no indices.

#ifdef __HUGS__
allocChars :: Int -> IO (PrimMutableByteArray RealWorld)
allocChars size = primNewByteArray size

-- Allocate a mutable array of words with no indices

allocWords :: Int -> IO (PrimMutableByteArray RealWorld)
allocWords size = primNewByteArray size
#else
allocChars :: Int -> IO (MutableByteArray RealWorld ())
allocChars (I# size#) = IO $ \ s# ->
    case newCharArray# size# s# of 
      (# s2#, barr# #) -> 
	(# s2#, MutableByteArray bot barr# #)
  where
    bot = error "Time.allocChars"

-- Allocate a mutable array of words with no indices

allocWords :: Int -> IO (MutableByteArray RealWorld ())
allocWords (I# size#) = IO $ \ s# ->
    case newIntArray# size# s# of 
      (# s2#, barr# #) -> 
	(# s2#, MutableByteArray bot barr# #)
  where
    bot = error "Time.allocWords"
#endif
\end{code}

\begin{code}
calendarTimeToString  :: CalendarTime -> String
calendarTimeToString  =  formatCalendarTime defaultTimeLocale "%c"

formatCalendarTime :: TimeLocale -> String -> CalendarTime -> String
formatCalendarTime l fmt (CalendarTime year mon day hour min sec _
                                       wday yday tzname _ _) =
        doFmt fmt
  where doFmt ('%':c:cs) = decode c ++ doFmt cs
        doFmt (c:cs) = c : doFmt cs
        doFmt "" = ""

        decode 'A' = fst (wDays l  !! fromEnum wday) -- day of the week, full name
        decode 'a' = snd (wDays l  !! fromEnum wday) -- day of the week, abbrev.
        decode 'B' = fst (months l !! fromEnum mon)  -- month, full name
        decode 'b' = snd (months l !! fromEnum mon)  -- month, abbrev
        decode 'h' = snd (months l !! fromEnum mon)  -- ditto
        decode 'C' = show2 (year `quot` 100)         -- century
        decode 'c' = doFmt (dateTimeFmt l)           -- locale's data and time format.
        decode 'D' = doFmt "%m/%d/%y"
        decode 'd' = show2 day                       -- day of the month
        decode 'e' = show2' day                      -- ditto, padded
        decode 'H' = show2 hour                      -- hours, 24-hour clock, padded
        decode 'I' = show2 (to12 hour)               -- hours, 12-hour clock
        decode 'j' = show3 yday                      -- day of the year
        decode 'k' = show2' hour                     -- hours, 24-hour clock, no padding
        decode 'l' = show2' (to12 hour)              -- hours, 12-hour clock, no padding
        decode 'M' = show2 min                       -- minutes
        decode 'm' = show2 (fromEnum mon+1)          -- numeric month
        decode 'n' = "\n"
        decode 'p' = (if hour < 12 then fst else snd) (amPm l) -- am or pm
        decode 'R' = doFmt "%H:%M"
        decode 'r' = doFmt (time12Fmt l)
        decode 'T' = doFmt "%H:%M:%S"
        decode 't' = "\t"
        decode 'S' = show2 sec			     -- seconds
        decode 's' = show2 sec			     -- number of secs since Epoch. (ToDo.)
        decode 'U' = show2 ((yday + 7 - fromEnum wday) `div` 7) -- week number, starting on Sunday.
        decode 'u' = show (let n = fromEnum wday in  -- numeric day of the week (1=Monday, 7=Sunday)
                           if n == 0 then 7 else n)
        decode 'V' =                                 -- week number (as per ISO-8601.)
            let (week, days) =                       -- [yep, I've always wanted to be able to display that too.]
                   (yday + 7 - if fromEnum wday > 0 then 
                               fromEnum wday - 1 else 6) `divMod` 7
            in  show2 (if days >= 4 then
                          week+1 
                       else if week == 0 then 53 else week)

        decode 'W' =				     -- week number, weeks starting on monday
            show2 ((yday + 7 - if fromEnum wday > 0 then 
                               fromEnum wday - 1 else 6) `div` 7)
        decode 'w' = show (fromEnum wday)            -- numeric day of the week, weeks starting on Sunday.
        decode 'X' = doFmt (timeFmt l)               -- locale's preferred way of printing time.
        decode 'x' = doFmt (dateFmt l)               -- locale's preferred way of printing dates.
        decode 'Y' = show year                       -- year, including century.
        decode 'y' = show2 (year `rem` 100)          -- year, within century.
        decode 'Z' = tzname                          -- timezone name
        decode '%' = "%"
        decode c   = [c]


show2, show2', show3 :: Int -> String
show2 x = [intToDigit (x `quot` 10), intToDigit (x `rem` 10)]

show2' x = if x < 10 then [ ' ', intToDigit x] else show2 x

show3 x = intToDigit (x `quot` 100) : show2 (x `rem` 100)

to12 :: Int -> Int
to12 h = let h' = h `mod` 12 in if h' == 0 then 12 else h'
\end{code}

Useful extensions for formatting TimeDiffs.

\begin{code}
timeDiffToString :: TimeDiff -> String
timeDiffToString = formatTimeDiff defaultTimeLocale "%c"

formatTimeDiff :: TimeLocale -> String -> TimeDiff -> String
formatTimeDiff l fmt (TimeDiff year month day hour min sec _)
 = doFmt fmt
  where 
   doFmt ""         = ""
   doFmt ('%':c:cs) = decode c ++ doFmt cs
   doFmt (c:cs)     = c : doFmt cs

   decode spec =
    case spec of
      'B' -> fst (months l !! fromEnum month)
      'b' -> snd (months l !! fromEnum month)
      'h' -> snd (months l !! fromEnum month)
      'C' -> show2 (year `quot` 100)
      'D' -> doFmt "%m/%d/%y"
      'd' -> show2 day
      'e' -> show2' day
      'H' -> show2 hour
      'I' -> show2 (to12 hour)
      'k' -> show2' hour
      'l' -> show2' (to12 hour)
      'M' -> show2 min
      'm' -> show2 (fromEnum month + 1)
      'n' -> "\n"
      'p' -> (if hour < 12 then fst else snd) (amPm l)
      'R' -> doFmt "%H:%M"
      'r' -> doFmt (time12Fmt l)
      'T' -> doFmt "%H:%M:%S"
      't' -> "\t"
      'S' -> show2 sec
      's' -> show2 sec -- Implementation-dependent, sez the lib doc..
      'X' -> doFmt (timeFmt l)
      'x' -> doFmt (dateFmt l)
      'Y' -> show year
      'y' -> show2 (year `rem` 100)
      '%' -> "%"
      c   -> [c]

\end{code}

\begin{code}
#ifdef __HUGS__
foreign import ccall "libHS_cbits.so" "get_tm_sec"   get_tm_sec   :: Bytes -> IO Int
foreign import ccall "libHS_cbits.so" "get_tm_min"   get_tm_min   :: Bytes -> IO Int
foreign import ccall "libHS_cbits.so" "get_tm_hour"  get_tm_hour  :: Bytes -> IO Int
foreign import ccall "libHS_cbits.so" "get_tm_mday"  get_tm_mday  :: Bytes -> IO Int
foreign import ccall "libHS_cbits.so" "get_tm_mon"   get_tm_mon   :: Bytes -> IO Int
foreign import ccall "libHS_cbits.so" "get_tm_year"  get_tm_year  :: Bytes -> IO Int
foreign import ccall "libHS_cbits.so" "get_tm_wday"  get_tm_wday  :: Bytes -> IO Int
foreign import ccall "libHS_cbits.so" "get_tm_yday"  get_tm_yday  :: Bytes -> IO Int
foreign import ccall "libHS_cbits.so" "get_tm_isdst" get_tm_isdst :: Bytes -> IO Int

foreign import ccall "libHS_cbits.so" "prim_ZONE"    prim_ZONE    :: Bytes -> IO Addr
foreign import ccall "libHS_cbits.so" "prim_GMTOFF"  prim_GMTOFF  :: Bytes -> IO Int

foreign import ccall "libHS_cbits.so" "prim_SETZONE" prim_SETZONE :: Bytes -> Bytes -> IO Int

foreign import ccall "libHS_cbits.so" "sizeof_word"      sizeof_word      :: Int
foreign import ccall "libHS_cbits.so" "sizeof_struct_tm" sizeof_struct_tm :: Int
foreign import ccall "libHS_cbits.so" "sizeof_time_t"    sizeof_time_t    :: Int

-- believed to be at least 1 bit (the sign bit!) bigger than sizeof_time_t
sizeof_int64 :: Int
sizeof_int64 = 8

foreign import ccall "libHS_cbits.so" "prim_getClockTime" prim_getClockTime :: Bytes -> Bytes -> IO Int
foreign import ccall "libHS_cbits.so" "prim_toClockSec"   prim_toClockSec   :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bytes -> IO Int
foreign import ccall "libHS_cbits.so" "prim_toLocalTime"  prim_toLocalTime  :: Int64 -> Bytes -> IO Int
foreign import ccall "libHS_cbits.so" "prim_toUTCTime"    prim_toUTCTime    :: Int64 -> Bytes -> IO Int
#endif
\end{code}
