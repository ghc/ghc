-- Double.pi doesn't work
-- Produces error
-- > *** Vectorisation error ***
-- >     Variable not vectorised: Data.Array.Parallel.Prelude.Double.pi

{-# LANGUAGE ParallelArrays, ParallelListComp #-}
{-# OPTIONS -fvectorise #-}
module Pi where

import Data.Array.Parallel hiding ((+), (-), (*), (/))
import Data.Array.Parallel.PArray
import Data.Array.Parallel.Prelude.Bool          as B
import Data.Array.Parallel.Prelude.Double        as D
import qualified Data.Array.Parallel.Prelude.Int as I
import qualified Data.Vector                     as V
import qualified Prelude                         as P

timesPi = mapP (D.* D.pi)

