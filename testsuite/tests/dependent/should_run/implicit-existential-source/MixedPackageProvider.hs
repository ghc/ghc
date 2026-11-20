{-# LANGUAGE ExperimentalDependentTypes, PolyKinds, RequiredTypeArguments, ScopedTypeVariables #-}
module MixedPackageProvider
  ( Inner, Mixed, inner, returnInner, written, lazyInner, ignoreMixed, readMixed
  , kindMixed, readKind
  ) where

import Data.Proxy (Proxy(..))
import qualified GHC.Exists as Exists

type Inner a = exists b | (a, b, Int)
type Mixed = exists a. Inner a

inner :: Inner Bool
inner = Exists.Pack (type Int) (True, 7 :: Int, 11)

returnInner :: Inner Bool -> Inner Bool
returnInner x = x

written :: Mixed
written = Exists.Pack @(Inner Bool) (type Int) (True, 7 :: Int, 13)

lazyInner :: Inner Bool
lazyInner = Exists.Pack (type Int) (error "mixed payload")

ignoreMixed :: Mixed -> String
ignoreMixed p = case p of
  Exists.Pack (type b) (_ :: (a, b, Int)) -> "ignored"

readMixed :: Mixed -> Int
readMixed p = case p of
  Exists.Pack (type b) (_, _ :: b, n) -> n

type KindInner k = exists (b :: k) | Proxy b
type KindMixed = exists k. KindInner k

kindMixed :: KindMixed
kindMixed = Exists.Pack @(KindInner Bool) (type True) Proxy

readKind :: KindMixed -> Bool
readKind p = case p of
  Exists.Pack (type b) (_ :: Proxy b) -> True
