{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}

-- Non-overloaded string literals checked in T23738_basic.
{-# LANGUAGE OverloadedStrings #-}

-- Non-overloaded lists are checked in T23738_basic.
{-# LANGUAGE OverloadedLists #-}

module T23738_overlit where

checkEq :: forall a b -> (a ~ b) => ()
checkEq (type _) (type _) = ()

result :: ()
result =
  checkEq
  {- type syntax: -} (type '("Hello", [1, 2, 3]))
  {- term syntax: -}        ("Hello", [1, 2, 3])