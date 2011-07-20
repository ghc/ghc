{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module T3965 where

import Data.Data

data T f e = Inl (f e) deriving (Data, Eq)

instance (Typeable1 f) => Typeable1 (T f) where
  typeOf1 _ = error "urk"

newtype Expr f = In (f (Expr f))
instance Typeable1 f => Typeable (Expr f) where
  typeOf _ = error "urk"

deriving instance (Typeable1 a, Data (a (Expr a))) => Data (Expr a)

data Var e = Var String deriving (Data, Eq, Typeable)

data Domain e g = Domain
    (Expr (T Var))
    deriving (Data, Typeable)


