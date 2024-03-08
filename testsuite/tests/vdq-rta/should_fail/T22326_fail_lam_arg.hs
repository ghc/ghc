{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}

module T22326_fail_lam_arg where

f :: forall (a :: k) -> ()
f (type _) = ()

x = f (\_ -> _)