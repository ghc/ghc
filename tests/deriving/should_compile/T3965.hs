{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module T3965 where

import Data.Data

data T f e = Inl (f e) deriving (Data, Typeable, Eq)

newtype Expr f = In (f (Expr f)) deriving Typeable

deriving instance (Typeable a, Data (a (Expr a))) => Data (Expr a)

data Var e = Var String deriving (Data, Eq, Typeable)

data Domain e g = Domain
    (Expr (T Var))
    deriving (Data, Typeable)


