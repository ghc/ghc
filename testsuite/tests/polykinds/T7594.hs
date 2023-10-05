{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE Rank2Types      #-}

module T7594 where

import Data.Kind (Constraint, Type)

class    (c1 t, c2 t) => (:&:) (c1 :: Type -> Constraint)
                               (c2 :: Type -> Constraint)
                               (t :: Type)
instance (c1 t, c2 t) => (:&:) c1 c2 t

data ColD c where
  ColD :: (c a) => a -> ColD c

app :: (forall a. (c a) => a -> b) -> ColD c -> b
app f (ColD x) = f x

q :: ColD (Show :&: Real)
q = ColD (1.2 :: Double)

bar = app print q
-- This one works, as a result of fixing #8644,
-- because the given constraint is
--   (Show :&: Real) a, which has no equality superclasses

q2 :: ColD (c :&: Real)
q2 = error "urk"

bar2 = app print q2
-- This one fail, because the given constraint is
--   (c :&: Real) a, which might have equality superclasses
