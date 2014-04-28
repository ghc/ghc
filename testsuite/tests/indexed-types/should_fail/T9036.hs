{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}


module T9036 where

class UncurryM t where
  type GetMonad t :: * -> *

class Curry a b where
  type Curried a b :: *

gSimple :: String -> String -> [String]
gSimple = simpleLogger (return ())

simpleLogger :: Maybe (GetMonad t after) -> t `Curried` [t]
simpleLogger _  _ = undefined
