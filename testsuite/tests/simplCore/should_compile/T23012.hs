{-# LANGUAGE BangPatterns, FlexibleInstances, MultiParamTypeClasses #-}

module T23012 where

import Data.Kind (Type)

class Vector v a where
  nothing :: v a
  just :: a -> v a

data Proxy (a :: Type) = P

instance Vector Proxy a where
  nothing = P
  just _ = P

step :: Maybe a
step = Nothing
{-# INLINE[0] step #-}

stream :: Vector v a => v a
stream = case step of
           Nothing -> nothing
           Just !x -> just x
{-# INLINE[1] stream #-}

data Id a = MkId a

f :: (Proxy (Id a), Proxy a)
f = (stream, stream)
