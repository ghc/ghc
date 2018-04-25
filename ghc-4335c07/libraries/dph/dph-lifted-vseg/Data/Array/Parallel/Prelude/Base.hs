{-# OPTIONS_GHC -fvectorise #-}

-- |This module sets up the basic vectorisation map for vectorising the DPH Prelude.
module Data.Array.Parallel.Prelude.Base
        ( PArr
        -- , (->), ()
        , Bool(..)
        , Ordering(..)
        , Eq(..), Ord(..)
        , Show
        , Num(..)
        )
where
import Data.Array.Parallel.Prim ()       -- dependency required by the vectoriser

import Data.Array.Parallel.PArr
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.Lifted.Closure


-- internal types
{-# VECTORISE type PArr = PArray #-}
{-# VECTORISE type PArray = PArray #-}
{-# VECTORISE SCALAR type (->) = (:->) #-}

-- vectorised versions of types from the standard Prelude
{-# VECTORISE SCALAR type () #-}
{-# VECTORISE SCALAR type Bool #-}
{-# VECTORISE SCALAR type Ordering #-}

-- FIXME: currently a fake definition to allow 'Integer' in vectorised classes
{-# VECTORISE SCALAR type Integer = Integer #-}

-- vectorised versions of type classes from the standard Prelude
{-# VECTORISE class Eq #-}
{-# VECTORISE class Ord #-}
{-# VECTORISE class Show #-}  -- only to facilitate 'Num', no vectorised instances provided  
{-# VECTORISE class Num #-}
