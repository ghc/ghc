{-# LANGUAGE ConstraintKinds, DataKinds, TypeOperators, GADTs #-}
{-# LANGUAGE TypeFamilies, KindSignatures, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Reproducer where

import Data.Kind (Type, Constraint)

data Dict (c :: Constraint) where
  Dict :: c => Dict c

class Foo (e :: Type) (r :: [Type])

instance Foo e (e ': r)

type family R :: [Type]
type family F (a :: [Type]) :: [Type]

compiles :: (R ~ Int ': F R, r ~ R)
         => Dict (Foo Int R)
compiles = Dict

errors :: (R ~ Int ': F R)
       => Dict (Foo Int R)
errors = Dict
