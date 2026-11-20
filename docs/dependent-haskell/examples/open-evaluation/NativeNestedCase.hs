{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, Modifiers, PolyKinds, TypeFamilies #-}
module NativeNestedCase
  ( Pick, Pair(..), LeftFirst, IgnoreLeft, Uniform, SameFallback
  , Indexed(..), IndexedPick, TypeHead, UniformTypeHead, Wrapped, WrappedBool
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import GHC.Exts (U)

type Pick d = case d of
  Just (Just field) -> Proxy field
  Just Nothing -> Int
  Nothing -> Char

data Pair = Pair (Maybe Bool) (Maybe Bool)

type LeftFirst d = case d of
  Pair (Just _) Nothing -> Int
  Pair _ (Just _) -> Bool

type IgnoreLeft d = case d of
  Pair _ (Just _) -> Bool
  Pair (Just _) Nothing -> Int

type SameFallback d = case d of
  Just (Just _) -> Int
  Just _ -> Int
  Nothing -> Char

data Indexed k where
  BooleanTag :: Bool -> Indexed Bool
  OrderingTag :: Ordering -> Indexed Ordering

type IndexedPick =
  ((\tag -> case tag of
      Just (BooleanTag field) -> Proxy field
      Just (OrderingTag field) -> Proxy field
      Nothing -> Char) :: forall k. @U Maybe (Indexed k) %U -> Type)

type Uniform =
  ((\tag -> case tag of
      Just (BooleanTag _) -> Int
      Just (OrderingTag _) -> Int
      Nothing -> Int) :: forall k. @U Maybe (Indexed k) %U -> Type)

type TypeHead (m :: Maybe Type) = case m of
  Just Bool -> Int
  Just Ordering -> Char
  Just _ -> ()
  Nothing -> ()

type Flag = Bool
type UniformTypeHead (m :: Maybe Type) = case m of
  Just Flag -> Int
  Just Ordering -> Int
  Just _ -> Int
  Nothing -> Int

type Optional a = Maybe a
type Wrapped (m :: Maybe Type) = case m of
  Just (Optional a) -> Proxy a
  Just _ -> ()
  Nothing -> ()

type WrappedBool (m :: Maybe Type) = case m of
  Just (Optional Bool) -> Int
  Just _ -> ()
  Nothing -> ()
