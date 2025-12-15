{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}

module T23738_wild where

idv :: forall a -> a -> a
idv (type t) (x :: t) = x

rBool = idv _ True
rChar = idv _ 'x'

rMaybeBool = idv (Maybe _) (Just True)
rMaybeChar = idv (Maybe _) (Just 'x')