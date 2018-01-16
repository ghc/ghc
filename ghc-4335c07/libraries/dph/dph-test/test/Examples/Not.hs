-- Lifted negation produces runtime error
--
{-# LANGUAGE ParallelArrays, ParallelListComp #-}
import NotV

import Data.Array.Parallel.PArray
import qualified Data.Vector                     as V


main = do
    let n   = toVector notV
    V.mapM_ print n
