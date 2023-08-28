{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}

module T23738_fail_implicit_tv where

vfun :: forall (a :: k) -> ()
vfun (type _) = ()

rNothing = vfun (Nothing :: Maybe a)
rLeft = vfun (Left :: a -> Either a b)