{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}

module T23738_sigforall where

import Data.Kind (Type)

checkEq :: forall a b -> (a ~ b) => ()
checkEq (type _) (type _) = ()

r1 :: ()
r1 =
  checkEq
  {- type syntax: -} (type (Left :: forall a b. a -> Either a b))
  {- term syntax: -}       (Left :: forall a b. a -> Either a b)

r2 :: ()
r2 =
  checkEq
  {- type syntax: -} (type (Left :: forall a. forall b. a -> Either a b))
  {- term syntax: -}       (Left :: forall a. forall b. a -> Either a b)

r3 :: ()
r3 =
  checkEq
  {- type syntax: -} (type (Left :: forall. forall a. forall b. a -> Either a b))
  {- term syntax: -}       (Left :: forall. forall a. forall b. a -> Either a b)