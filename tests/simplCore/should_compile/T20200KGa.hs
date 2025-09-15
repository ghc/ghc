module T20200KGa where

import {-# SOURCE #-} T20200KG

getOpen :: (Subst a, Monad m) => Open a -> m a
getOpen (OpenThing x) = do
  sub <- checkpointSubstitution x
  return $ applySubst sub x

data Open a = OpenThing a

class Subst a where
  applySubst ::  a -> a -> a

instance Subst Int where
  applySubst _ = id
