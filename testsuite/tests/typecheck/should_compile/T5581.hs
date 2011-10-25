{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, KindSignatures #-}
module TcShouldTerminate where

class C (p :: Constraint)
class D (p :: Constraint)

instance C (D p) => C (D (D p))
