{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE Rank2Types      #-}
module T7594 where

import GHC.Prim (Constraint)

class    (c1 t, c2 t) => (:&:) (c1 :: * -> Constraint) (c2 :: * -> Constraint) (t :: *)
instance (c1 t, c2 t) => (:&:) c1 c2 t

data ColD c where
  ColD :: (c a) => a -> ColD c

app :: (forall a. (c a) => a -> b) -> ColD c -> b
app f (ColD x) = f x

q :: ColD (Show :&: Real)
q = ColD (1.2 :: Double)

bar = app print q


