{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, TypeFamilies #-}
module NativeTypeLetContexts where

import Data.Proxy (Proxy(..))

type family G a where
  G a = Maybe a

signature :: forall a. Proxy a -> let b = G a in Either b b
signature _ = Left (Nothing @a)

data Field a = Field (let b = G a in Either b b)

readField :: Field a -> Either (Maybe a) (Maybe a)
readField (Field value) = value

data Indexed a where
  At :: Proxy a -> Indexed (let b = G a in Either b b)

index :: Proxy a -> Indexed (Either (Maybe a) (Maybe a))
index = At

type Alias a = let b = G a in Either b b

alias :: forall a. Proxy a -> Alias a
alias _ = Left (Nothing @a)

type family Closed a where
  Closed (Maybe a) = let b = G a in Either b b

closed :: forall a. Proxy a -> Closed (Maybe a)
closed _ = Left (Nothing @a)

type family Open a
type instance Open (Maybe a) = let b = G a in Either b b

open :: forall a. Proxy a -> Open (Maybe a)
open _ = Left (Nothing @a)
