{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module T15380 where

import Data.Kind

class Generic a where
  type Rep a :: Type

class PGeneric a where
  type To a (x :: Rep a) :: a

type family MDefault (x :: a) :: a where
  MDefault x = To (M x)

class C a where
  type M (x :: a) :: a
  type M (x :: a) = MDefault x
