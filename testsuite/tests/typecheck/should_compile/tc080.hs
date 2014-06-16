--module Parse(Parse(..),whiteSpace,seperatedBy) where
--import StdLib
module ShouldSucceed where

import Data.Char

class Parse a where
       parseFile :: String -> [a]
       parseLine :: String -> a
       parseType :: String -> (a,String)
       parse :: String -> (a,String)
       forced :: a -> Bool

       parseFile string | all forced x = x
		       where x = map parseLine (lines' string)
       parseLine = pl.parse where pl (a,_) = a
       parse = parseType.whiteSpace
       forced x = True

instance Parse Int where
       parseType str = pl (span' isDigit str)
	       where 	pl (l,r) = (strToInt l,r)
       forced n | n>=0 = True

instance Parse Char where
       parseType (ch:str) = (ch,str)
       forced n = True

instance (Parse a) => Parse [a] where
	parseType more = (map parseLine (seperatedBy ',' (l++",")),out)
		       where 	(l,']':out) = span' (\x->x/=']') (tail more)
	forced = all forced

seperatedBy :: Char -> String -> [String]
seperatedBy ch [] = []
seperatedBy ch xs = twaddle ch (span' (\x->x/=ch) xs)
	       where	twaddle ch (l,_:r) = l:seperatedBy ch r

whiteSpace :: String -> String
whiteSpace = dropWhile isSpace

span' :: (a->Bool) -> [a] -> ([a],[a])
span' p [] = ([],[])
span' p (x:xs') | p x = fixLeak x (span' p xs') where fixLeak x (xs,ys) = (x:xs,ys)
span' _ xs = ([],xs)

lines' :: [Char] -> [[Char]]
lines' "" = []
lines' s = plumb (span' ((/=) '\n') s)
       where   plumb (l,s') = l:if null s' then [] else lines' (tail s')

strToInt :: String -> Int
strToInt x = strToInt' (length x-1) x
      where   strToInt' _ [] = 0
	      strToInt' x (a:l) = (charToInt a)*(10^x) + (strToInt' (x-1) l)

charToInt :: Char -> Int
charToInt x = (ord x - ord '0')
