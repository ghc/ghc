{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module T3101 where

type family F a :: *

data Boom = Boom (forall a. a -> F a)
  deriving Show
