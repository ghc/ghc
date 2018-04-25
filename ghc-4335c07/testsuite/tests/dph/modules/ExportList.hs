-- Explicit export list
-- Produces error
-- > ghc-stage2: panic! (the 'impossible' happened)
-- >   (GHC version 7.7.20130109 for x86_64-unknown-linux):
-- >      nameModule solveV{v r3Ep}
-- It is something about internal vs external names.

{-# LANGUAGE ParallelArrays, ParallelListComp #-}
{-# OPTIONS -fvectorise #-}
module ExportList (solvePA) where

import Data.Array.Parallel hiding ((+), (-), (*), (/))
import Data.Array.Parallel.PArray
import Data.Array.Parallel.Prelude.Bool          as B
import Data.Array.Parallel.Prelude.Double        as D
import qualified Data.Array.Parallel.Prelude.Int as I
import qualified Data.Vector                     as V
import qualified Prelude                         as P

data NodeV = NodeV Double Double Double [:NodeV:]

{-# NOINLINE solvePA #-}
solvePA
    :: NodeV    -- ^ nodes
    -> Double   -- ^ time
    -> PArray Double
solvePA nodes t = toPArrayP (solveV t)


solveV :: Double -> [:Double:]
solveV t
 = concatP (mapP solveV [: :])

