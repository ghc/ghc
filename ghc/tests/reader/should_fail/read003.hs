-- !!! Irrefutable patterns + guards
module Read003 where

~(a,b,c) | nullity b	= a
	 | nullity c	= a
	 | otherwise	= a
	 where
	    nullity = null
