{-# OPTIONS_GHC -fvectorise #-}

-- |This module sets up the basic vectorisation map for vectorising the DPH Prelude.

module Data.Array.Parallel.Prelude.Base
  ( PArr
  -- , ()
  , Bool
  )
where

import Data.Array.Parallel.Prim ()       -- dependency required by the vectoriser

import Data.Array.Parallel.PArr
import Data.Array.Parallel.Lifted.Closure

{-# VECTORISE type PArr = PArray #-}
{-# VECTORISE type PArray = PArray #-}

{-# VECTORISE SCALAR type (->) = (:->) #-}

{-# VECTORISE SCALAR type () #-}
{-# VECTORISE SCALAR type Bool #-}
