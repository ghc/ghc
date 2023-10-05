{-# LANGUAGE PolyKinds, KindSignatures, DataKinds, GADTs #-}
module T5937 where

import Data.Kind (Type)

data SMaybe :: (k -> Type) -> Maybe k -> Type where
   SNothing :: SMaybe s 'Nothing
   SJust :: s a -> SMaybe s ('Just a)
