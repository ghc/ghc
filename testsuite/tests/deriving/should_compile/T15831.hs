{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module T15831 where

import Data.Functor.Const (Const(..))
import GHC.Exts (Any)

newtype Age = MkAge Int
  deriving Eq
    via Const Int Any
  deriving Ord
    via Const Int (Any :: k)
  deriving Read
    via (forall k. Const Int (Any :: k))
  deriving Show
    via Const Int a
  deriving Enum
    via Const Int (a :: k)
  deriving Bounded
    via (forall a. Const Int a)
  deriving Num
    via (forall k (a :: k). Const Int a)

newtype Age2 = MkAge2 Int
deriving via                     Const Int Any         instance Eq      Age2
deriving via                     Const Int (Any :: k)  instance Ord     Age2
deriving via (forall k.          Const Int (Any :: k)) instance Read    Age2
deriving via                     Const Int a           instance Show    Age2
deriving via                     Const Int (a :: k)    instance Enum    Age2
deriving via (forall a.          Const Int a)          instance Bounded Age2
deriving via (forall k (a :: k). Const Int a)          instance Num     Age2
