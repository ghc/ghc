
{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
module Vectorised where
import Data.Array.Parallel              hiding (Bool, True, False, not)
import Data.Array.Parallel.Prelude      hiding (Bool, True, False, not)
import Data.Array.Parallel.Prelude.Int
import qualified Prelude        as P

data Thing      = True | False
data Bool       = Thing Thing

toBool :: Int -> Bool
toBool n
 | n == 0       = Thing False
 | otherwise    = Thing True

fromBool :: Bool -> Int
fromBool (Thing False)  = 0
fromBool (Thing True)   = 1

not :: Bool -> Bool
not (Thing False)       = Thing True
not (Thing True)        = Thing False

{-# NOINLINE test #-}
test :: PArray Int -> PArray Int
test xs = toPArrayP (mapP test' (fromPArrayP xs))
test' x = fromBool (not (toBool x))
