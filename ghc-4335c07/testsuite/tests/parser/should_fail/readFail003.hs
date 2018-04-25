-- !!! Irrefutable patterns + guards
module Read003 where
import GHC.List; import Prelude hiding (null)
~(a,b,c) | nullity b    = a
         | nullity c    = a
         | otherwise    = a
         where
            nullity = null
