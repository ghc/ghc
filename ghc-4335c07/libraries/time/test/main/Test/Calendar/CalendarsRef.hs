module Test.Calendar.CalendarsRef where

testCalendarsRef :: String
testCalendarsRef =
 unlines
  [ " == MJD -678576 == Gregorian 0000-12-31 == Julian 0001-01-02 == ISO 8601 0000-W52-7"
  , " == MJD -38780 == Gregorian 1752-09-13 == Julian 1752-09-02 == ISO 8601 1752-W37-3"
  , " == MJD -38779 == Gregorian 1752-09-14 == Julian 1752-09-03 == ISO 8601 1752-W37-4"
  , " == MJD 53393 == Gregorian 2005-01-23 == Julian 2005-01-10 == ISO 8601 2005-W03-7" ]
