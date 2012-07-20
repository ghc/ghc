{-# LANGUAGE TypeFamilies #-}
module T7082 where

class R m where
  type D m a :: *
  type D m a = ()                -- (1)
  f :: m a -> D m a -> ()

instance R Maybe where
  -- type D Maybe a = ()         -- (2)
  f = undefined

x = f (Nothing :: Maybe Int) ()
