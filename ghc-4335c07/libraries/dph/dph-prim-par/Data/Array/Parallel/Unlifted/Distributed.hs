-- | Distributed types and operations.
--
--   * This is an internal API and shouldn't need to be used directly.
--     Client programs should use "Data.Array.Parallel.Unlifted"
--
module Data.Array.Parallel.Unlifted.Distributed 
        ( -- * Gang operations
          Gang, forkGang, gangSize

          -- * Gang hacks
        , theGang

          -- * Distributed types and classes
        , DT(..)

          -- * Higher-order combinators
        , mapD, zipWithD
        , foldD
        , scanD

          -- * Equality
        , eqD, neqD

          -- * Distributed scalars
        , scalarD
        , andD, orD
        , sumD

          -- * Distributed pairs
        , zipD, unzipD
        , fstD, sndD
        , zip3D

          -- * Distributed arrays
        , lengthD
        , splitLenD
        , splitLenIdxD
        , splitD
        , splitAsD
        , joinLengthD
        , joinD
        , splitJoinD
        , joinDM
        , carryD

        , Distribution
        , balanced
        , unbalanced

          -- * Permutations
        , permuteD, bpermuteD

          -- * Update
        , atomicUpdateD

          -- * Debugging
        , fromD, toD, debugD)
where
import Data.Array.Parallel.Unlifted.Distributed.Combinators
import Data.Array.Parallel.Unlifted.Distributed.Data.Bool
import Data.Array.Parallel.Unlifted.Distributed.Data.Tuple
import Data.Array.Parallel.Unlifted.Distributed.Data.Scalar
import Data.Array.Parallel.Unlifted.Distributed.Data.Ordering   ()
import Data.Array.Parallel.Unlifted.Distributed.Arrays
import Data.Array.Parallel.Unlifted.Distributed.Basics
import Data.Array.Parallel.Unlifted.Distributed.Primitive

