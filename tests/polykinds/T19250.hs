{-# LANGUAGE TypeOperators, TypeFamilies, ConstraintKinds, PolyKinds, DataKinds, EmptyDataDecls #-}

module T19250 where

import Data.Kind

type Exp a = a -> Type

type family Eval (e :: Exp a) :: a

data Collapse :: [Constraint] -> Exp Constraint
type instance Eval (Collapse '[]) = ()
