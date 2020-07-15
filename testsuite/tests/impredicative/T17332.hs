{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Bug where

import GHC.Exts

data Dict c where
  MkDict :: c => Dict c

aux :: Dict (forall a. a)
aux = MkDict

{-
[W] forall (c:: Constraint). c
==>
  forall c. [W] c
-}
