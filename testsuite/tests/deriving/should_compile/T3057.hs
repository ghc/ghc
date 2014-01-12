{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
module T3057 where

import T3057A

deriving instance Functor (MyType a)


