{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}

module T22326_fail_raw_pat where

f :: forall (a :: k) -> ()
f x = ()