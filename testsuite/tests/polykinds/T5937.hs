{-# LANGUAGE PolyKinds, KindSignatures, DataKinds, GADTs #-}
module T5937 where

data SMaybe :: (k -> *) -> Maybe k -> * where
   SNothing :: SMaybe s 'Nothing
   SJust :: s a -> SMaybe s ('Just a)
