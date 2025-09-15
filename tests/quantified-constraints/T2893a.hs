{-# LANGUAGE
    GADTs
  , FlexibleContexts
  , RankNTypes
  , ScopedTypeVariables
  , QuantifiedConstraints #-}

module T2893a where

import Control.Monad.ST
import Data.Array.ST

sortM
  :: forall a s.
     (Ord a, MArray (STUArray s) a (ST s))
  => [a]
  -> ST s [a]
sortM xs = do
  arr <- newListArray (1, length xs) xs
           :: ST s (STUArray s Int a)
  -- do some in-place sorting here
  getElems arr

sortP_3
  :: (Ord a, forall s. MArray (STUArray s) a (ST s))
  => [a] -> [a]
sortP_3 xs = runST (sortM xs)
