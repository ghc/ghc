ReadLists
Written by Sam Waugh
Date Started : 10th September 1992
Last Modified: 10th November 1992

This module allows the reading of lists of values from a string
of the one type seperated by white space.

Thanks to Paul Hudak for suggestions concerning getVals.

> module ReadLists (readWhiteList, readNumBools) where


readWhiteList reads a white-spaced list from a given string

> readWhiteList :: (Read a) => String -> [a]
> readWhiteList = getVals reads


readNumBools reads a list of white-spaced boolean values from a given
string.  Booleans in a string are represented as 1's and 0's.

> readNumBools :: String -> [Bool]
> readNumBools = getVals readBool

> readBool :: ReadS Bool
> readBool []     = []
> readBool (x:xs) = [(x == '1', xs)]


getVals (base function) takes a string, s, and a reading function, readVal,
and repeatedly applies readVal to s while removing whitespace

> getVals :: ReadS a -> String -> [a]
> getVals readVal s = case readVal (stripWhite s) of
>                       []       -> []
>                       (x,s'):_ -> x : getVals readVal s'


stripWhite removes white space from the front of a string

> stripWhite :: String -> String
> stripWhite = dropWhile (`elem` " \t\n")
