{-# LANGUAGE LinearTypes #-}
module T21278 where

import GHC.Types (Multiplicity)

data C a = forall (p :: Multiplicity). C (a %p -> a)

f :: C a -> C a
f b = C (\x -> case b of C g -> g x)
