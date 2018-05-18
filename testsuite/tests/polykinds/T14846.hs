{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
module T14846 where

import Data.Kind
import Data.Proxy

type Cat ob = ob -> ob -> Type

data Struct :: (k -> Constraint) -> Type where
  S :: Proxy (a::k) -> Struct (cls::k -> Constraint)

type Structured a cls = (S ('Proxy :: Proxy a)::Struct cls)

data AStruct :: Struct cls -> Type where
  AStruct :: cls a => AStruct (Structured a cls)

class StructI xx (structured::Struct (cls :: k -> Constraint)) where
  struct :: AStruct structured

instance (Structured xx cls ~ structured, cls xx) => StructI xx structured where
  struct :: AStruct (Structured xx cls)
  struct = AStruct

data Hom :: Cat k -> Cat (Struct cls) where

class Category (cat::Cat ob) where
  i :: StructI xx a => ríki a a

instance Category riki => Category (Hom riki :: Cat (Struct cls)) where
  i :: forall xx a. StructI xx a => Hom riki a a
  i = case struct :: AStruct (Structured a cls) of
