{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module LocalOpen (Selected, keepFirst, LastOf, lastOf, LazyFirst, lazyFirst, Routed, route) where

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

-- | The type chosen by the ordinary selector below.
type LazyFirst :: Type -> Type -> Type
type LazyFirst a b = lazyFirst a b

-- | Return the first argument. The recursive second argument stays unevaluated.
lazyFirst :: a -> a -> a
lazyFirst x y = select x (loopAgain y)

-- | Diverge if a caller demands this argument.
loopAgain :: a -> a
loopAgain x = loopAgain x

-- | Select a type using a constructor from this module.
type Routed :: Type -> Type -> Type
type Routed a b = LocalOpen.route a b

-- | Use the same routing function on ordinary values.
route :: a -> a -> a
route x y = follow Second x y

-- | Select which argument to return.
data Route = First | Second

-- | An ordinary result synonym used by the helper's signature.
type ResultOf a = a

-- | Return the field selected by the route.
follow :: Route -> a -> a -> ResultOf a
follow First x _ = x
follow Second _ y = y
