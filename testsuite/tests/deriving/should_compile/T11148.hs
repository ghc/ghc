{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE FlexibleInstances  #-}

module T11148 where

data family G a b c d
data instance G Int b Float d = G deriving Functor

data family H a b c d
data instance H [b] b d c = H deriving Functor
