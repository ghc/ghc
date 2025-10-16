
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

{-# LANGUAGE UnboxedSums, UnboxedTuples, MagicHash #-}

module T26465d where

-- Should-fail variant of T26465c (but with -fdefer-type-errors)

import Data.Kind
  ( Constraint )
import GHC.Exts
  ( TYPE )

type HasP :: forall r. TYPE r -> Constraint
class HasP a where
  getP :: a -> (# (# #) | (# #) #)
  mk :: (# #) -> a

g = getP
m = mk

-- NB: deliberately use no arguments to make this test harder (so that we run
-- into the 'need_dummy_arg' logic of 'GHC.Tc.TyCl.PatSyn.mkPatSynBuilder').
pattern P1 <- ( g -> (# | (# #) #) )
  where P1 = m (# #)

test P1 = P1
