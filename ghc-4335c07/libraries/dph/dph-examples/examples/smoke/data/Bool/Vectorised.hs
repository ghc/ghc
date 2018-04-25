
{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
module Vectorised (test) where
import Data.Array.Parallel              hiding (Bool, True, False, not, fromBool, toBool)
import Data.Array.Parallel.Prelude      hiding (Bool, True, False, not, fromBool, toBool)
import Data.Array.Parallel.Prelude.Int  as I
import qualified Prelude        as P
        
data Bool = True | False

toBool :: Int -> Bool
toBool n
 | n I.== 0     = False
 | otherwise    = True

fromBool :: Bool -> Int
fromBool False  = 0
fromBool True   = 1

not :: Bool -> Bool
not False       = True
not True        = False

{-# NOINLINE test #-}
test :: PArray Int -> PArray Int
test xs = toPArrayP (mapP test' (fromPArrayP xs))
test' x = fromBool (not (toBool x))
