{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

module T11966 where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

-- Simplification
type family Col (f :: k -> j) (x :: k) :: Type

-- Base types
data PGBaseType = PGInteger | PGText

-- Transformations
data Column t = Column Symbol t
newtype Nullable t = Nullable t
newtype HasDefault t = HasDefault t

-- Interpretations
data Expr k

data Record (f :: forall k. k -> Type) =
  Record {rX :: Col f ('Column "x" 'PGInteger)
         ,rY :: Col f ('Column "y" ('Nullable 'PGInteger))
         ,rZ :: Col f ('HasDefault 'PGText)}

x :: Record Expr
x = undefined
