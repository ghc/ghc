%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[LibTime]{Haskell 1.3 Time of Day Library}

The {\em LibTime} library provides the functionality of "time.h",
adapted to the Haskell environment.  It includes timezone information,
as in System V, and follows RFC 1129 in its use of Coordinated
Universal Time (UTC).

\begin{code}
module LibTime (
	CalendarTime(..),
	ClockTime,
	TimeDiff(..),
	addToClockTime,
	diffClockTimes,
	getClockTime,
	toCalendarTime,
	toUTCTime,
	toClockTime
    ) where

import PreludeIOError
import PreludeGlaST
import PS

\end{code}

$ClockTime$ is an abstract type, used for the internal clock time.
Clock times may be compared, converted to strings, or converted to an
external calendar time $CalendarTime$.

\begin{code}
data ClockTime = TOD Integer Integer
                 deriving (Eq, Ord)
\end{code}

When a $ClockTime$ is shown, it is converted to a string of the form
$"Mon Nov 28 21:45:41 GMT 1994"$.

For now, we are restricted to roughly:
Fri Dec 13 20:45:52 1901 through Tue Jan 19 03:14:07 2038, because
we use the C library routines based on 32 bit integers.

\begin{code}
instance Text ClockTime where
    showsPrec p (TOD sec@(J# a# s# d#) nsec) = 
        showString (unsafePerformPrimIO (
	    _ccall_ showTime (I# s#) (_ByteArray (error "ClockTime.show") d#)
						    `thenPrimIO` \ str ->
            _ccall_ strlen str			    `thenPrimIO` \ len ->
            _packCBytesST len str		    `thenStrictlyST` \ ps ->
            returnPrimIO (_unpackPS ps)))
\end{code}


$CalendarTime$ is a user-readable and manipulable
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
--                   year mon  day  hour min  sec  picosec wday yday tzname tz  isdst
data CalendarTime = 
       CalendarTime  Int  Int  Int  Int  Int  Int  Integer Int  Int  String Int Bool
\end{code}

The $TimeDiff$ type records the difference between two clock times in
a user-readable way.

\begin{code}
--                          year mon  day  hour min  sec  picosec
data TimeDiff    = TimeDiff Int  Int  Int  Int  Int  Int  Integer
                   deriving (Eq,Ord)
\end{code}

$getClockTime$ returns the current time in its internal representation.

\begin{code}
getClockTime :: IO ClockTime
getClockTime =
    malloc1					    `thenStrictlyST` \ i1 ->
    malloc1					    `thenStrictlyST` \ i2 ->
    _ccall_ getClockTime i1 i2			    `thenPrimIO` \ rc ->
    if rc == 0 then
	cvtUnsigned i1				    `thenStrictlyST` \ sec ->
	cvtUnsigned i2				    `thenStrictlyST` \ nsec ->
	return (TOD sec (nsec * 1000))
    else
	_constructError				    `thenPrimIO` \ ioError ->
	failWith ioError
  where
    malloc1 (S# s#) =
	case newIntArray# 1# s# of 
          StateAndMutableByteArray# s2# barr# -> (_MutableByteArray bot barr#, S# s2#)
    bot = error "getClockTime"

    -- The C routine fills in an unsigned word.  We don't have `unsigned2Integer#,'
    -- so we freeze the data bits and use them for an MP_INT structure.  Note that
    -- zero is still handled specially, although (J# 1# 1# (ptr to 0#)) is probably
    -- acceptable to gmp.

    cvtUnsigned (_MutableByteArray _ arr#) (S# s#) =
	case readIntArray# arr# 0# s# of 
	  StateAndInt# s2# r# ->
            if r# ==# 0# then
                (0, S# s2#)
            else
                case unsafeFreezeByteArray# arr# s2# of
                  StateAndByteArray# s3# frozen# -> (J# 1# 1# frozen#, S# s3#)

\end{code}

$addToClockTime$ {\em d} {\em t} adds a time difference {\em d} and a
clock time {\em t} to yield a new clock time.  The difference {\em d}
may be either positive or negative.  $diffClockTimes$ {\em t1} {\em
t2} returns the difference between two clock times {\em t1} and {\em
t2} as a $TimeDiff$.


\begin{code}
addToClockTime  :: TimeDiff  -> ClockTime -> ClockTime
addToClockTime _ _ = error "addToClockTime unimplemented"

diffClockTimes  :: ClockTime -> ClockTime -> TimeDiff
diffClockTimes _ _ = error "diffClockTimes unimplemented"
\end{code}

$toCalendarTime$ {\em t} converts {\em t} to a local time, modified by
the current timezone and daylight savings time settings.  $toUTCTime$
{\em t} converts {\em t} into UTC time.  $toClockTime$ {\em l}
converts {\em l} into the corresponding internal $ClockTime$.  The
{\em wday}, {\em yday}, {\em tzname}, and {\em isdst} fields are
ignored.

\begin{code}
toCalendarTime :: ClockTime -> CalendarTime
toCalendarTime (TOD sec@(J# a# s# d#) psec) = unsafePerformPrimIO (
    _ccall_ toLocalTime (I# s#) (_ByteArray (error "toCalendarTime") d#)
						    `thenPrimIO` \ tm ->
    if tm == (``NULL''::_Addr) then
	error "toCalendarTime{LibTime}: out of range"
    else
	_casm_ ``%r = ((struct tm *)%0)->tm_sec;'' tm
						    `thenPrimIO` \ sec ->
	_casm_ ``%r = ((struct tm *)%0)->tm_min;'' tm
						    `thenPrimIO` \ min ->
	_casm_ ``%r = ((struct tm *)%0)->tm_hour;'' tm
						    `thenPrimIO` \ hour ->
	_casm_ ``%r = ((struct tm *)%0)->tm_mday;'' tm
						    `thenPrimIO` \ mday ->
	_casm_ ``%r = ((struct tm *)%0)->tm_mon;'' tm
						    `thenPrimIO` \ mon ->
	_casm_ ``%r = ((struct tm *)%0)->tm_year;'' tm
						    `thenPrimIO` \ year ->
	_casm_ ``%r = ((struct tm *)%0)->tm_wday;'' tm
						    `thenPrimIO` \ wday ->
	_casm_ ``%r = ((struct tm *)%0)->tm_yday;'' tm
						    `thenPrimIO` \ yday ->
	_casm_ ``%r = ((struct tm *)%0)->tm_isdst;'' tm
						    `thenPrimIO` \ isdst ->
	_ccall_ ZONE tm				    `thenPrimIO` \ zone ->
	_ccall_ GMTOFF tm			    `thenPrimIO` \ tz ->
        _ccall_ strlen zone			    `thenPrimIO` \ len ->
        _packCBytesST len zone			    `thenStrictlyST` \ tzname ->
        returnPrimIO (CalendarTime (1900+year) mon mday hour min sec psec 
                      wday yday (_unpackPS tzname) tz (isdst /= 0))
    )

toUTCTime :: ClockTime -> CalendarTime
toUTCTime  (TOD sec@(J# a# s# d#) psec) = unsafePerformPrimIO (
        _ccall_ toUTCTime (I# s#) (_ByteArray (error "toCalendarTime") d#)
						    `thenPrimIO` \ tm ->
    if tm == (``NULL''::_Addr) then
	error "toUTCTime{LibTime}: out of range"
    else
	_casm_ ``%r = ((struct tm *)%0)->tm_sec;'' tm
						    `thenPrimIO` \ sec ->
	_casm_ ``%r = ((struct tm *)%0)->tm_min;'' tm
						    `thenPrimIO` \ min ->
	_casm_ ``%r = ((struct tm *)%0)->tm_hour;'' tm
						    `thenPrimIO` \ hour ->
	_casm_ ``%r = ((struct tm *)%0)->tm_mday;'' tm
						    `thenPrimIO` \ mday ->
	_casm_ ``%r = ((struct tm *)%0)->tm_mon;'' tm
						    `thenPrimIO` \ mon ->
	_casm_ ``%r = ((struct tm *)%0)->tm_year;'' tm
						    `thenPrimIO` \ year ->
	_casm_ ``%r = ((struct tm *)%0)->tm_wday;'' tm
						    `thenPrimIO` \ wday ->
	_casm_ ``%r = ((struct tm *)%0)->tm_yday;'' tm
						    `thenPrimIO` \ yday ->
        returnPrimIO (CalendarTime (1900+year) mon mday hour min sec psec 
                      wday yday "UTC" 0 False)
    )

toClockTime :: CalendarTime -> ClockTime
toClockTime (CalendarTime year mon mday hour min sec psec wday yday tzname tz isdst) =
    if psec < 0 || psec > 999999999999 then
        error "toClockTime{LibTime}: picoseconds out of range"
    else if tz < -43200 || tz > 43200 then
        error "toClockTime{LibTime}: timezone offset out of range"
    else
        unsafePerformPrimIO (
	    _ccall_ toClockSec year mon mday hour min sec tz
						    `thenPrimIO` \ ptr@(A# ptr#) ->
            if ptr /= ``NULL'' then
		returnPrimIO (TOD (int2Integer# (indexIntOffAddr# ptr# 0#)) psec)
	    else
		error "toClockTime{LibTime}: can't perform conversion"
        )
\end{code}

