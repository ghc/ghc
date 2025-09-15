{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}

-- Overloaded string literals checked in T23738_overlit.
{-# LANGUAGE NoOverloadedStrings #-}

module T23738_basic where

type (:+:) = Either

checkEq :: forall a b -> (a ~ b) => ()
checkEq (type _) (type _) = ()

result :: ()
result =
  checkEq
  {- type syntax: -} (type '(Maybe Int, String :+: Int, 10, "Hello", 'x' :: Char, '[] @Bool, '[10], [1, 2]))
  {- term syntax: -}        (Maybe Int, String :+: Int, 10, "Hello", 'x' :: Char,  [] @Bool,  [10], [1, 2])