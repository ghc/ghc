{-# LANGUAGE RequiredTypeArguments #-}

module RequiredTypeArgumentsMkSymCo where

import Data.Kind (Type)

f :: forall a . forall (b :: Type) -> a -> a
f t = id
{-# INLINE f #-}
