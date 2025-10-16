
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

{-# LANGUAGE UnboxedSums, UnboxedTuples, MagicHash #-}

module T26465c where

-- Rep-poly variant of T26465b

import Data.Kind
  ( Constraint )
import GHC.Exts
  ( TYPE, Int#, isTrue#, (>=#) )


type HasP :: forall r. TYPE r -> Constraint
class HasP a where
  getP :: a -> (# (# #) | (# #) #)
  mk :: (# #) -> a

instance HasP Int where
  getP i = if i >= 0 then (# | (# #) #) else (# (# #) | #)
  mk _ = 1
instance HasP Int# where
  getP i# = if isTrue# ( i# >=# 0# ) then (# | (# #) #) else (# (# #) | #)
  mk _ = 1#

g1 = getP
g2 = getP

m1 = mk
m2 = mk

-- NB: deliberately use no arguments to make this test harder (so that we run
-- into the 'need_dummy_arg' logic of 'GHC.Tc.TyCl.PatSyn.mkPatSynBuilder').
pattern P1 <- ( g1 -> (# | (# #) #) )
  where P1 = m1 (# #)
pattern P2 <- ( g2 -> (# | (# #) #) )
  where P2 = m2 (# #)

y1 :: Int -> Int
y1 P1 = P1

y2 :: Int# -> Int#
y2 P2 = P2
