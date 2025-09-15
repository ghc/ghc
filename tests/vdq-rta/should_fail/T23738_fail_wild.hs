{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NamedWildCards #-}

module T23738_fail_wild where

idv :: forall a -> a -> a
idv (type t) (x :: t) = x

rBool' = idv _w True
rChar' = idv _w 'x'

rMaybeBool' = idv (Maybe _w) (Just True)
rMaybeChar' = idv (Maybe _w) (Just 'x')