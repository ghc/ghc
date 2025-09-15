{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}

module T22326_fail_caseof where

f :: Int -> Bool
f x =
  case x of
    (type _) -> True
    _        -> False