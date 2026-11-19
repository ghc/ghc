{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module LocalOpen (Selected, keepFirst, LastOf, lastOf) where

import Data.Kind (Type)

-- | The ordinary function below determines which input type is selected.
type Selected :: Type -> Type -> Type
type Selected a b = LocalOpen.select a b

-- | Preserve the chosen value type in an ordinary client.
keepFirst :: forall a b. a -> b -> Selected a b
keepFirst x _ = x

-- | Choose the first operand without inspecting the second.
select :: a -> a -> a
select x _ = x

-- | Select the second type using the same definition that runs on values.
type LastOf :: Type -> Type -> Type
type LastOf a b = lastOf a b

-- | Reverse the inputs to the local first-argument selector.
lastOf :: a -> a -> a
lastOf x y = select y x
