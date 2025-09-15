{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}

module T22326_fail_notInScope where

x = notInScope (type Int)
