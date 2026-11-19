{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module LocalOpen (Selected, keepFirst) where

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
