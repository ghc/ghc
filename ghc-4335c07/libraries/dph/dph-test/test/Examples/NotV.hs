{-# LANGUAGE ParallelArrays, ParallelListComp #-}
{-# OPTIONS -fvectorise #-}
module NotV where

import Data.Array.Parallel hiding ((+), (-), (*), (/))
import Data.Array.Parallel.PArray
import Data.Array.Parallel.Prelude.Bool          as B
import Data.Array.Parallel.Prelude.Double        as D
import qualified Data.Array.Parallel.Prelude.Int as I
import qualified Data.Vector                     as V
import qualified Prelude                         as P


notV =
    let bs  = [: x I.> 5 | x <- I.enumFromToP 1 10 :]
        bs' = mapP B.not bs
    in  toPArrayP bs'
    
