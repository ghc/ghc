{-# LANGUAGE AllowAmbiguousTypes, ExperimentalDependentTypes, PolyKinds, RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments, ScopedTypeVariables, TypeFamilies #-}
module MixedPackageProvider
  ( Inner, Mixed, inner, returnInner, written, lazyInner, ignoreMixed, readMixed
  , kindMixed, readKind, chooseMixed, readInner, ignoreInner, outerValue, readKindInner
  , RankedMixed, rankedMixed, readRankInner, BadRank, FamilyMixed, ignoreFamily
  ) where

import Data.Kind (Type)
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

chooseMixed :: Bool -> Mixed
chooseMixed True = written
chooseMixed False = Exists.Pack @(Inner Int) (type Bool) (17, False, 21)

readInner :: forall a. Inner a -> Int
readInner p = case p of
  Exists.Pack _ (_, _, n) -> n

ignoreInner :: forall a. Inner a -> Bool
ignoreInner _ = True

outerValue :: forall a. Inner a -> a
outerValue p = case p of
  Exists.Pack _ (x, _, _) -> x

readKindInner :: forall k. KindInner k -> Bool
readKindInner _ = True

type RankInner a = exists b | (forall c. c -> (a, b, c))
type RankedMixed = exists a. RankInner a

rankInner :: RankInner Bool
rankInner = Exists.Pack (type Int) (\x -> (True, 7, x))

rankedMixed :: RankedMixed
rankedMixed = rankInner

readRankInner :: forall a. RankInner a -> (Int, Bool)
readRankInner p = case p of
  Exists.Pack _ f ->
    let (_, _, n) = f (17 :: Int)
        (_, _, b) = f True
    in (n, b)

type BadRank = exists (a :: Type). exists b | (forall c. c -> (c, b, c))

type family FamilyValue (a :: Type) :: Type
type FamilyInner a = exists b | (FamilyValue a, b)
type FamilyMixed = exists a. FamilyInner a

ignoreFamily :: forall a. FamilyInner a -> Bool
ignoreFamily _ = True
