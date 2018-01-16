 module Parse(Parse(..),whiteSpace,seperatedBy) where
 import Data.Char -- 1.3
 import StdLib
 class Parse a where
	parseFile :: String -> [a]
	parseLine :: String -> a
 	parse :: String -> (a,String)
 	parseType :: String -> (a,String)
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
