{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module T23739_th_dump1 where

$([d| f :: Integer -> forall a -> Num a => a
      f n t = fromInteger n :: t
    |])

$([d| x = 42 `f` Double
      n = f 42 Integer
    |])