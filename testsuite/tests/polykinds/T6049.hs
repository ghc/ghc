{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, GADTs,  ExistentialQuantification #-}

module T6049 where

import Data.Kind (Type)

data SMaybe :: (k -> Type) -> Maybe k -> Type where
   SNothing :: forall k (s :: k -> Type). SMaybe s Nothing
   SJust :: forall k (s :: k -> Type) (a :: k). SMaybe s (Just a)

