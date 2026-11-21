{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
module LocalPunBindings where

import Prelude hiding (id)

id :: forall a -> a -> a
id (type a) a = a

invisible :: t -> t
invisible @b b = b

lambda :: forall a -> a -> a
lambda = \(type c) c -> c

unrelated :: forall a -> a -> a
unrelated (type t) value = value

nested :: t -> t
nested @a = \(a :: a) -> a

ordinaryShadow :: a -> a
ordinaryShadow a = let f a = a in f a

freshAnnotation :: t -> t
freshAnnotation (d :: d) = d
