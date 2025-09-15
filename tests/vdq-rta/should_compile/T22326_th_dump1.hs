{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module T22326_th_dump1 where

$([d| f :: Integer -> forall a -> Num a => a
      f n (type t) = fromInteger n :: t
    |])

$([d| x = 42 `f` (type Double)
      n = f 42 (type Integer)
    |])