{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module T15215 where

import Data.Kind

data A :: Type -> Type where
  MkA :: Show (Maybe a) => A a

data B :: Type -> Type where
  MkB :: Show a => B a

data SA :: forall a. A a -> Type where
  SMkA :: SA MkA
  SMkB :: SA MkB
