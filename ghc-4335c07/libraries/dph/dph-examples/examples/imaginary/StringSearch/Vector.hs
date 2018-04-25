module Vector
	( searchV )
where
import qualified Data.Vector            as V
import qualified Data.Vector.Unboxed    as U
import Prelude hiding (String)


type String	= U.Vector Char

next_character :: U.Vector Int -> String -> String -> Int -> U.Vector Int
next_character candidates w s i
 | i == U.length w = candidates
 | otherwise
 = let letter      = w U.! i
       next_l      = U.map (\ix -> s U.! (ix + i)) candidates
       (candidates',_) = U.unzip (U.filter (\(_,n) -> n == letter) (candidates `U.zip` next_l))
   in  next_character candidates' w s (i + 1)

string_search :: String -> String -> U.Vector Int
string_search w s = next_character (U.enumFromN 0 (U.length s - U.length w + 1)) w s 0

-- Interface ------------------------------------------------------------------

-- | SEARCH
{-# NOINLINE searchV #-}
searchV :: String -> String -> U.Vector Int
searchV w s = string_search w s

