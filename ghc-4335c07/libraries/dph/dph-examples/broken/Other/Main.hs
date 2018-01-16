
-- | Test vectorisation of enumerations.
--   This tests the conversion to and from our generic representation.
import Vectorised
import qualified Data.Array.Parallel.PArray     as P

main    = print $ P.toList $ test $ P.fromList [0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1]