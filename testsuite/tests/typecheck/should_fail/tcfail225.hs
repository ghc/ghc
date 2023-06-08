{-# LANGUAGE PolyKinds, GADTs #-}

module TcFail225 where

import Data.Kind (Type)

data T (m :: k -> Type) :: k -> Type where
  MkT :: m a -> T Maybe (m a) -> T m a
