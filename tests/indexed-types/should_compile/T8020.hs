{-# LANGUAGE TypeFamilies #-}

module T8020 where

type family F a b where
  F (Maybe a) [a] = Int
  F b c           = Bool

data Proxy a = P

type family G

foo :: Proxy d -> F d d -> Bool
foo _ = not

bar :: Bool -> Bool
bar = foo (P :: Proxy G)
