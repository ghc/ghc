{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedNewtypes #-}

module UnliftedNewtypesDifficultUnification where

import GHC.Exts
import Data.Kind

data Color = Red | Blue

type family Interpret (x :: Color) :: RuntimeRep where
  Interpret 'Red = 'IntRep
  Interpret 'Blue = 'WordRep

data family Foo (x :: Color) :: TYPE (Interpret x)
newtype instance Foo 'Red = FooRedC Int#

newtype Quux :: TYPE (Interpret Red) where
  MkQ :: Int# -> Quux

newtype instance Foo 'Blue :: TYPE WordRep where
  MkFB :: Word# -> Foo 'Blue

type family Lower (x :: Type) :: RuntimeRep where
  Lower Int = IntRep
  Lower Word = WordRep

data family Bar (x :: Color) :: TYPE (Interpret x)

newtype instance Bar 'Red :: TYPE (Lower Int) where
  MkBR :: Int# -> Bar 'Red
