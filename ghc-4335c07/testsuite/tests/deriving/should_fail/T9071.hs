{-# LANGUAGE DeriveFunctor #-}
module T9071 where

import T9071a

newtype K a b = K a
newtype F a = F (Mu (K a)) deriving Functor

