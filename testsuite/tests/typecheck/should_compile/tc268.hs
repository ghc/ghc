{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
module Tc268 where
import GHC.Exts
type A = (() :: Constraint)
