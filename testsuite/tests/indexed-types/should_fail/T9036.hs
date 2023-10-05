{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module T9036 where

import Data.Kind

class UncurryM t where
  type GetMonad t :: Type -> Type

class Curry a b where
  type Curried a b :: Type

gSimple :: String -> String -> [String]
gSimple = simpleLogger (return ())

simpleLogger :: Maybe (GetMonad t after) -> t `Curried` [t]
simpleLogger _  _ = undefined
