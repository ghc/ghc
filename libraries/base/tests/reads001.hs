-- Test the classic "\SOH" ambiguity

module Main(main) where

main = do { print soh ; print (length (fst (head soh))) ;
	    print so  ; print (length (fst (head so))) }
     where
	so, soh :: [(String,String)]
 	soh = reads "\"\\SOH\""	-- Should read \SOH
	so  = reads "\"\\SOx\""	-- Should read \SO followed by x
