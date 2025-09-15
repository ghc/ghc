{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
module T13813 where

import GHC.Exts (Constraint)

type C (a :: Constraint) b = a

data T a b = C (Show a) b => MkT b
deriving instance Functor (T a)
