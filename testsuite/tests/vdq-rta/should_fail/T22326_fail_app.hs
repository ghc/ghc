{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}

module T22326_fail_app where

xhead = (type Maybe) undefined
xarg  = undefined (type Int)