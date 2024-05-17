{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module Bug1050 where

import Data.Kind

newtype T :: (forall k. k -> Type) -> (forall k. k -> Type) where
  MkT :: forall (f :: forall k. k -> Type) k (a :: k). f a -> T f a

mkT = MkT
