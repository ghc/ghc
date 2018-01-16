-- List enumeration doesn't work for parallel list comprehensions.
--
-- > ghc-stage2: panic! (the 'impossible' happened)
-- >   (GHC version 7.7.20130109 for x86_64-unknown-linux):
-- >      DsMonad: uninitialised ds_parr_bi
--
-- (I.enumFromToP is a workaround)

{-# LANGUAGE ParallelArrays, ParallelListComp #-}
{-# OPTIONS -fvectorise #-}
module EnumFromToP where

import Data.Array.Parallel hiding ((+), (-), (*), (/))
import Data.Array.Parallel.PArray
import Data.Array.Parallel.Prelude.Bool          as B
import Data.Array.Parallel.Prelude.Double        as D
import qualified Data.Array.Parallel.Prelude.Int as I
import qualified Data.Vector                     as V
import qualified Prelude                         as P


nums = [: 0 .. 100 :]
    

