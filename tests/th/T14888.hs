{-# LANGUAGE TemplateHaskell #-}
module T14888 where

import Language.Haskell.TH

foo :: $([t| (->) Bool Bool |])
foo x = x

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' ((->) r) where
  fmap' = (.)

$(return [])

functor'Instances :: String
functor'Instances = $(reify ''Functor' >>= stringE . pprint)
