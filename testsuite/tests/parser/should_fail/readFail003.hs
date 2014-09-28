-- !!! Irrefutable patterns + guards
module Read003 where
import Data.OldList; import Prelude hiding (null)
~(a,b,c) | nullity b	= a
	 | nullity c	= a
	 | otherwise	= a
	 where
	    nullity = null
