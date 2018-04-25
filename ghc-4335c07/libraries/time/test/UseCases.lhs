> module UseCases where
> import Data.Time.Calendar.OrdinalDate
> import Data.Time
> import System.Locale


From Brian Smith:
<http://www.haskell.org/pipermail/libraries/2005-July/004060.html>

Use cases (primarily taken from real-world corporate IT applications I have
developed) :

* What is the equivalent (or closest aproximation) of the SQL DateTime type
(date and time without any timezone information)? What is the equivalent of
the SQL Date type (date without any timezone information)?

> type SQLDateTime = LocalTime
> type SQLDate = Day

* The user enters a date as "7/4/2005." How do I determine if this date is
before or after July 1st of this year?

TODO: Parsing

* How do I present the date "July 1st of this year" to the user in M/D/YYYY
format?

> july1st = do
>    now <- getZonedTime
>    let (thisYear,_,_) = toGregorian (localDay (zonedTimeToLocalTime now))
>    let day = fromGregorian thisYear 7 1
>    return (formatTime defaultTimeLocale "%m/%d/%Y" day)

This actually gives "07/01/2005" rather than "7/1/2005".
ISSUE: Should I make additional %-codes for this?


* How do I truncate a datetime to midnight of the same day? How do I
truncate a date to the first of the month? How do I truncate a date to the
first day of the year it occurred in?

> truncateToMidnight (LocalTime day _) = (LocalTime day midnight)

> truncateToFirstOfMonth day = fromGregorian y m 1 where
>    (y,m,_) = toGregorian day

> truncateToJan1st day = fromOrdinalDate y 1 where
>    (y,_) = toOrdinalDate day

* Given a date X, how do I find the last day of the month that X occurs in.
For example, If X is July 4th, 2005, then I want the result to be July 31st,
2005. If X is Februrary 5, then I want the result to be Februrary 28 for
non-leap-years and February 29 for leap years.

> lastDayOfMonth day = fromGregorian y m (gregorianMonthLength y m) where
>    (y,m,_) = toGregorian day

* The user enters a time T with no date, e.g. "17:30". How do I merge this
time onto a date D (e.g. July 4, 2005), so that the result has is a datetime
with date D and the time T (July 4, 2005 at 17:30).

> mergeDateAndTime = LocalTime

* Given two datetimes T1, T2, how do I determine if they are on the same
date?

> sameDay (LocalTime d1 _) (LocalTime d2 _) = d1 == d2


From Simon Marlow:
<http://www.haskell.org/pipermail/libraries/2005-July/004066.html>

I just had a little look around, mainly at System.Time.Calendar.  I
think the structure is rather complicated - I wanted to find out how to
get a CalendarTime for "this time tomorrow", and ended up with this:

*System.Time.Calendar> let c' =
c{ztTime=zttime{dtDay=dtday{gregDay=day+1}}} where { zttime = ztTime c;
dtday = dtDay zttime; day = gregDay dtday }

> thisTimeTomorrow (ZonedTime (LocalTime day tod) zone) = (ZonedTime (LocalTime (addDays 1 day) tod) zone)


