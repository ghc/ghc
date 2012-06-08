{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, GADTs,  ExistentialQuantification #-}

module T6049 where

data SMaybe :: (k -> *) -> Maybe k -> * where
   SNothing :: forall (s :: k -> *). SMaybe s Nothing
   SJust :: forall (s :: k -> *) (a :: k). SMaybe s (Just a)

