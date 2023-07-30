{-# LANGUAGE RequiredTypeArguments #-}

module T23740j where

x :: ()
x = ()
  where
    a = a
    f :: a -> a
    f = f
