{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module T19044 where

class C a b where
  m :: a -> b

instance C a a where
  m = id

instance C a (Maybe a) where
  m _ = Nothing

f :: a -> Maybe a
f = g
  where
    g x = h (m x) x

h :: Maybe a -> a -> Maybe a
h = const
