{-# LANGUAGE ExperimentalDependentTypes, StandaloneKindSignatures, TypeFamilies #-}
module IndependentFamily (Payload, result) where

import Data.Kind (Type)

-- | Choose the payload type of an ordinary argument.
type family Payload a
type instance Payload Int = IndependentFamily.ident Bool
type instance Payload Char = Ordering

-- | Supply the input type to the integer equation.
ident :: forall a. a -> a
ident x = x

-- | Check the Boolean equation while checking the ignored argument.
second :: forall a. a -> a
second x = const x (True :: Payload Int)

-- | Apply the second producer after the family is available.
type Also :: Type -> Type
type Also a = IndependentFamily.second a

-- | Expose the result at its computed type.
result :: Also (Payload Int)
result = True
