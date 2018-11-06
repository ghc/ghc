{-# LANGUAGE TypeFamilies #-}
module T7082 where

import Data.Kind (Type)

class R m where
  type D m a :: Type
  type D m a = ()                -- (1)
  f :: m a -> D m a -> ()

instance R Maybe where
  -- type D Maybe a = ()         -- (2)
  f = undefined

x = f (Nothing :: Maybe Int) ()
