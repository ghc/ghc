{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module T23738_th where

type (:+:) = Either

checkEq :: forall a b -> (a ~ b) => ()
checkEq (type _) (type _) = ()

result :: ()
result =
  checkEq
  {- type syntax: -} (type $[t| '(Maybe Int, String :+: Int, 10, "Hello", 'x' :: Char, '[] @Bool, '[10], [1, 2]) |])
  {- term syntax: -}       $[e|  (Maybe Int, String :+: Int, 10, "Hello", 'x' :: Char,  [] @Bool,  [10], [1, 2]) |]