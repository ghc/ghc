{-# LANGUAGE ExperimentalDependentTypes, InstanceSigs, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, StandaloneKindSignatures #-}
module LocalInstance (Input, passValue) where

import Data.Kind (Type)

-- | A pass that can preserve a value at any input type.
class Pass p where
    enabled :: p -> Bool
    applyPass :: forall a. p -> a -> a

-- | Select the instance defined below.
data IdentityPass = IdentityPass

-- | Resolving the instance is sufficient here. The result ignores the method.
keep :: forall a. a -> a
keep x = const x (enabled IdentityPass)

-- | Use the local function in a type before checking the instance methods.
type Input :: Type -> Type
type Input a = LocalInstance.keep a

-- | The type of this producer uses the result of the earlier declaration.
later :: forall a. Input a -> a
later x = x

instance Pass IdentityPass where
    enabled _ = (True :: Input Bool)
    applyPass :: forall a. IdentityPass -> a -> a
    applyPass _ x = (x :: LocalInstance.later a)

-- | Run the checked instance from an ordinary client.
passValue :: forall a. a -> Input a
passValue = applyPass IdentityPass
