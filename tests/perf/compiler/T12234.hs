{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{- # OPTIONS_GHC -O1 #-}

module T12234 () where

import Prelude (Eq)

data ExprF rT = ExprF rT rT deriving Eq

newtype Expr   = Expr (Fix ExprF) deriving Eq
newtype Fix fT = In (fT (Fix fT))

deriving instance Eq (f (Fix f)) => Eq (Fix f)
