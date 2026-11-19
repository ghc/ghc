{-# LANGUAGE ExperimentalDependentTypes, StandaloneKindSignatures, TypeFamilies #-}
module LocalAssociated (result) where

import Data.Kind (Type)

-- | The instance chooses the result type of enabled.
class Gate a where
    type Flag a
    enabled :: a -> Flag a

-- | Select the instance whose flag is Boolean.
data Token = Token

-- | A local prerequisite of the associated equation.
type Answer = Bool

-- | Solving the constraint on not needs the associated equation.
keep :: forall a. a -> a
keep x = const x (not (enabled Token))

-- | Reuse the ordinary producer as a type.
type Kept :: Type -> Type
type Kept a = LocalAssociated.keep a

instance Gate Token where
    type Flag Token = Answer
    enabled _ = (True :: Kept Bool)

-- | Run the method after its signature and implementation have been checked.
result :: Kept Bool
result = enabled Token
