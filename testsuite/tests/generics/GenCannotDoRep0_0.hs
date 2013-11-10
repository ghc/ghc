{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DatatypeContexts           #-}

module CannotDoRep0_0 where

import GHC.Generics hiding (P, D)

-- We do not support existential quantification
data Dynamic = forall a. Dynamic a deriving Generic

-- Partial instantiation of types is not allowed
data P a = P a
deriving instance Generic (P Int)

-- This gets trickier for data families
data family D a b
data instance D Char b
data instance (Show b) => D Int b
data instance D () ()

-- Bad: second argument is instantiated
deriving instance Generic (D Char Char)
-- Bad: has context
deriving instance Generic (D Int a)
-- Ok
deriving instance Generic (D () ())
