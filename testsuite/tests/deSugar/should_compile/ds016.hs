-- !!! ds016 -- case expressions
--
module ShouldCompile where

f x y z =
    case ( x ++ x ++ x ++ x ++ x ) of
	[]	-> []
	[a]	-> error "2"
	[a,b,c]	->
		    case ( (y,z,y,z) ) of
--		      (True, _, False, _) | True == False -> z
--		      (True, _, False, _) | True == False -> z
		      _ -> z

	(a:bs)	-> error "4"
