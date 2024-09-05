{-# LANGUAGE LinearTypes #-}
module T20232 where

import GHC.Types (Multiplicity)

data C a = forall (p :: Multiplicity). C (a %p -> a)

f :: C a -> a %1 -> a
f b x = case b of C h -> h x
