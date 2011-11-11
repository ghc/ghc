{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, KindSignatures #-}
module TcShouldTerminate where

import GHC.Prim (Constraint)

class C (p :: Constraint)
class D (p :: Constraint)

instance C (D p) => C (D (D p))
