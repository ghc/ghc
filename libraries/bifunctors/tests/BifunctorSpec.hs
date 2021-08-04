{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE RoleAnnotations #-}
#endif

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-unused-foralls #-}
#endif

{-|
Module:      BifunctorSpec
Copyright:   (C) 2008-2015 Edward Kmett, (C) 2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Edward Kmett
Portability: Template Haskell

@hspec@ tests for the "Data.Bifunctor.TH" module.
-}
module BifunctorSpec where

import Data.Bifunctor
import Data.Bifunctor.TH
import Data.Bifoldable
import Data.Bitraversable

import Data.Char (chr)
import Data.Functor.Classes (Eq1, Show1)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Monoid

import GHC.Exts (Int#)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary)

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative (Applicative(..))
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
#endif

-------------------------------------------------------------------------------

-- Adapted from the test cases from
-- https://ghc.haskell.org/trac/ghc/attachment/ticket/2953/deriving-functor-tests.patch

-- Plain data types

data Strange a b c
    = T1 a b c
    | T2 [a] [b] [c]         -- lists
    | T3 [[a]] [[b]] [[c]]   -- nested lists
    | T4 (c,(b,b),(c,c))     -- tuples
    | T5 ([c],Strange a b c) -- tycons

type IntFun a b = (b -> Int) -> a
data StrangeFunctions a b c
    = T6 (a -> c)            -- function types
    | T7 (a -> (c,a))        -- functions and tuples
    | T8 ((b -> a) -> c)     -- continuation
    | T9 (IntFun b c)        -- type synonyms

data StrangeGADT a b where
    T10 :: Ord d            => d        -> StrangeGADT c d
    T11 ::                     Int      -> StrangeGADT e Int
    T12 :: c ~ Int          => c        -> StrangeGADT f Int
    T13 :: i ~ Int          => Int      -> StrangeGADT h i
    T14 :: k ~ Int          => k        -> StrangeGADT j k
    T15 :: (n ~ c, c ~ Int) => Int -> c -> StrangeGADT m n

data NotPrimitivelyRecursive a b
    = S1 (NotPrimitivelyRecursive (a,a) (b, a))
    | S2 a
    | S3 b

newtype OneTwoCompose f g a b = OneTwoCompose (f (g a b))
  deriving (Arbitrary, Eq, Show)

newtype ComplexConstraint f g a b = ComplexConstraint (f Int Int (g a,a,b))

data Universal a b
    = Universal  (forall b. (b,[a]))
    | Universal2 (forall f. Bifunctor f => f a b)
    | Universal3 (forall a. Maybe a) -- reuse a
    | NotReallyUniversal (forall b. a)

data Existential a b
    = forall a. ExistentialList [a]
    | forall f. Bitraversable f => ExistentialFunctor (f a b)
    | forall b. SneakyUseSameName (Maybe b)

data IntHash a b
    = IntHash Int# Int#
    | IntHashTuple Int# a b (a, b, Int, IntHash Int (a, b, Int))

data IntHashFun a b
    = IntHashFun ((((a -> Int#) -> b) -> Int#) -> a)

data Empty1 a b
data Empty2 a b
#if __GLASGOW_HASKELL__ >= 708
type role Empty2 nominal nominal
#endif

data TyCon81 a b
    = TyCon81a (forall c. c -> (forall d. a -> d) -> a)
    | TyCon81b (Int -> forall c. c -> b)

type family F :: * -> * -> *
type instance F = Either

data TyCon82 a b = TyCon82 (F a b)

-- Data families

data family   StrangeFam x  y z
data instance StrangeFam a  b c
    = T1Fam a b c
    | T2Fam [a] [b] [c]         -- lists
    | T3Fam [[a]] [[b]] [[c]]   -- nested lists
    | T4Fam (c,(b,b),(c,c))     -- tuples
    | T5Fam ([c],Strange a b c) -- tycons

data family   StrangeFunctionsFam x y z
data instance StrangeFunctionsFam a b c
    = T6Fam (a -> c)            -- function types
    | T7Fam (a -> (c,a))        -- functions and tuples
    | T8Fam ((b -> a) -> c)     -- continuation
    | T9Fam (IntFun b c)        -- type synonyms

data family   StrangeGADTFam x y
data instance StrangeGADTFam a b where
    T10Fam :: Ord d            => d        -> StrangeGADTFam c d
    T11Fam ::                     Int      -> StrangeGADTFam e Int
    T12Fam :: c ~ Int          => c        -> StrangeGADTFam f Int
    T13Fam :: i ~ Int          => Int      -> StrangeGADTFam h i
    T14Fam :: k ~ Int          => k        -> StrangeGADTFam j k
    T15Fam :: (n ~ c, c ~ Int) => Int -> c -> StrangeGADTFam m n

data family   NotPrimitivelyRecursiveFam x y
data instance NotPrimitivelyRecursiveFam a b
    = S1Fam (NotPrimitivelyRecursive (a,a) (b, a))
    | S2Fam a
    | S3Fam b

data family      OneTwoComposeFam (j :: * -> *) (k :: * -> * -> *) x y
newtype instance OneTwoComposeFam f g a b = OneTwoComposeFam (f (g a b))
  deriving (Arbitrary, Eq, Show)

data family      ComplexConstraintFam (j :: * -> * -> * -> *) (k :: * -> *) x y
newtype instance ComplexConstraintFam f g a b = ComplexConstraintFam (f Int Int (g a,a,b))

data family   UniversalFam x y
data instance UniversalFam a b
    = UniversalFam  (forall b. (b,[a]))
    | Universal2Fam (forall f. Bifunctor f => f a b)
    | Universal3Fam (forall a. Maybe a) -- reuse a
    | NotReallyUniversalFam (forall b. a)

data family   ExistentialFam x y
data instance ExistentialFam a b
    = forall a. ExistentialListFam [a]
    | forall f. Bitraversable f => ExistentialFunctorFam (f a b)
    | forall b. SneakyUseSameNameFam (Maybe b)

data family   IntHashFam x y
data instance IntHashFam a b
    = IntHashFam Int# Int#
    | IntHashTupleFam Int# a b (a, b, Int, IntHashFam Int (a, b, Int))

data family   IntHashFunFam x y
data instance IntHashFunFam a b
    = IntHashFunFam ((((a -> Int#) -> b) -> Int#) -> a)

data family   TyFamily81 x y
data instance TyFamily81 a b
    = TyFamily81a (forall c. c -> (forall d. a -> d) -> a)
    | TyFamily81b (Int -> forall c. c -> b)

data family   TyFamily82 x y
data instance TyFamily82 a b = TyFamily82 (F a b)

-------------------------------------------------------------------------------

-- Plain data types

$(deriveBifunctor     ''Strange)
$(deriveBifoldable    ''Strange)
$(deriveBitraversable ''Strange)

$(deriveBifunctor     ''StrangeFunctions)
$(deriveBifoldable    ''StrangeGADT)

$(deriveBifunctor     ''NotPrimitivelyRecursive)
$(deriveBifoldable    ''NotPrimitivelyRecursive)
$(deriveBitraversable ''NotPrimitivelyRecursive)

$(deriveBifunctor     ''OneTwoCompose)
$(deriveBifoldable    ''OneTwoCompose)
$(deriveBitraversable ''OneTwoCompose)

instance (Bifunctor (f Int), Functor g) =>
  Bifunctor (ComplexConstraint f g) where
    bimap = $(makeBimap ''ComplexConstraint)

instance (Bifoldable (f Int), Foldable g) =>
  Bifoldable (ComplexConstraint f g) where
    bifoldr   = $(makeBifoldr ''ComplexConstraint)
    bifoldMap = $(makeBifoldMap ''ComplexConstraint)

bifoldlComplexConstraint
  :: (Bifoldable (f Int), Foldable g)
  => (c -> a -> c) -> (c -> b -> c) -> c -> ComplexConstraint f g a b -> c
bifoldlComplexConstraint = $(makeBifoldl ''ComplexConstraint)

bifoldComplexConstraint
  :: (Bifoldable (f Int), Foldable g, Monoid m)
  => ComplexConstraint f g m m -> m
bifoldComplexConstraint = $(makeBifold ''ComplexConstraint)

instance (Bitraversable (f Int), Traversable g) =>
  Bitraversable (ComplexConstraint f g) where
    bitraverse = $(makeBitraverse ''ComplexConstraint)

bisequenceAComplexConstraint
  :: (Bitraversable (f Int), Traversable g, Applicative t)
  => ComplexConstraint f g (t a) (t b) -> t (ComplexConstraint f g a b)
bisequenceAComplexConstraint = $(makeBisequenceA ''ComplexConstraint)

$(deriveBifunctor     ''Universal)

$(deriveBifunctor     ''Existential)
$(deriveBifoldable    ''Existential)
$(deriveBitraversable ''Existential)

$(deriveBifunctor     ''IntHash)
$(deriveBifoldable    ''IntHash)
$(deriveBitraversable ''IntHash)

$(deriveBifunctor     ''IntHashFun)

$(deriveBifunctor     ''Empty1)
$(deriveBifoldable    ''Empty1)
$(deriveBitraversable ''Empty1)

-- Use EmptyCase here
$(deriveBifunctorOptions     defaultOptions{emptyCaseBehavior = True} ''Empty2)
$(deriveBifoldableOptions    defaultOptions{emptyCaseBehavior = True} ''Empty2)
$(deriveBitraversableOptions defaultOptions{emptyCaseBehavior = True} ''Empty2)

$(deriveBifunctor     ''TyCon81)

$(deriveBifunctor     ''TyCon82)
$(deriveBifoldable    ''TyCon82)
$(deriveBitraversable ''TyCon82)

#if MIN_VERSION_template_haskell(2,7,0)
-- Data families

$(deriveBifunctor     'T1Fam)
$(deriveBifoldable    'T2Fam)
$(deriveBitraversable 'T3Fam)

$(deriveBifunctor     'T6Fam)
$(deriveBifoldable    'T10Fam)

$(deriveBifunctor     'S1Fam)
$(deriveBifoldable    'S2Fam)
$(deriveBitraversable 'S3Fam)

$(deriveBifunctor     'OneTwoComposeFam)
$(deriveBifoldable    'OneTwoComposeFam)
$(deriveBitraversable 'OneTwoComposeFam)

instance (Bifunctor (f Int), Functor g) =>
  Bifunctor (ComplexConstraintFam f g) where
    bimap = $(makeBimap 'ComplexConstraintFam)

instance (Bifoldable (f Int), Foldable g) =>
  Bifoldable (ComplexConstraintFam f g) where
    bifoldr   = $(makeBifoldr 'ComplexConstraintFam)
    bifoldMap = $(makeBifoldMap 'ComplexConstraintFam)

bifoldlComplexConstraintFam
  :: (Bifoldable (f Int), Foldable g)
  => (c -> a -> c) -> (c -> b -> c) -> c -> ComplexConstraintFam f g a b -> c
bifoldlComplexConstraintFam = $(makeBifoldl 'ComplexConstraintFam)

bifoldComplexConstraintFam
  :: (Bifoldable (f Int), Foldable g, Monoid m)
  => ComplexConstraintFam f g m m -> m
bifoldComplexConstraintFam = $(makeBifold 'ComplexConstraintFam)

instance (Bitraversable (f Int), Traversable g) =>
  Bitraversable (ComplexConstraintFam f g) where
    bitraverse = $(makeBitraverse 'ComplexConstraintFam)

bisequenceAComplexConstraintFam
  :: (Bitraversable (f Int), Traversable g, Applicative t)
  => ComplexConstraintFam f g (t a) (t b) -> t (ComplexConstraintFam f g a b)
bisequenceAComplexConstraintFam = $(makeBisequenceA 'ComplexConstraintFam)

$(deriveBifunctor     'UniversalFam)

$(deriveBifunctor     'ExistentialListFam)
$(deriveBifoldable    'ExistentialFunctorFam)
$(deriveBitraversable 'SneakyUseSameNameFam)

$(deriveBifunctor     'IntHashFam)
$(deriveBifoldable    'IntHashTupleFam)
$(deriveBitraversable 'IntHashFam)

$(deriveBifunctor     'IntHashFunFam)

$(deriveBifunctor     'TyFamily81a)

$(deriveBifunctor     'TyFamily82)
$(deriveBifoldable    'TyFamily82)
$(deriveBitraversable 'TyFamily82)
#endif

-------------------------------------------------------------------------------

prop_BifunctorLaws :: (Bifunctor p, Eq (p a b), Eq (p c d), Show (p a b), Show (p c d))
                   => (a -> c) -> (b -> d) -> p a b -> Expectation
prop_BifunctorLaws f g x = do
    bimap  id id x `shouldBe` x
    first  id    x `shouldBe` x
    second id    x `shouldBe` x
    bimap  f  g  x `shouldBe` (first f . second g) x

prop_BifunctorEx :: (Bifunctor p, Eq (p [Int] [Int]), Show (p [Int] [Int])) => p [Int] [Int] -> Expectation
prop_BifunctorEx = prop_BifunctorLaws reverse (++ [42])

prop_BifoldableLaws :: (Eq a, Eq b, Eq z, Show a, Show b, Show z,
                        Monoid a, Monoid b, Bifoldable p)
                => (a -> b) -> (a -> b)
                -> (a -> z -> z) -> (a -> z -> z)
                -> z -> p a a -> Expectation
prop_BifoldableLaws f g h i z x = do
    bifold        x `shouldBe` bifoldMap id id x
    bifoldMap f g x `shouldBe` bifoldr (mappend . f) (mappend . g) mempty x
    bifoldr h i z x `shouldBe` appEndo (bifoldMap (Endo . h) (Endo . i) x) z

prop_BifoldableEx :: Bifoldable p => p [Int] [Int] -> Expectation
prop_BifoldableEx = prop_BifoldableLaws reverse (++ [42]) ((+) . length) ((*) . length) 0

prop_BitraversableLaws :: (Applicative f, Applicative g, Bitraversable p,
                           Eq   (g (p c c)), Eq   (p a b), Eq   (p d e), Eq1 f,
                           Show (g (p c c)), Show (p a b), Show (p d e), Show1 f)
                       => (a -> f c) -> (b -> f c) -> (c -> f d) -> (c -> f e)
                       -> (forall x. f x -> g x) -> p a b -> Expectation
prop_BitraversableLaws f g h i t x = do
    bitraverse (t . f) (t . g)   x `shouldBe` (t . bitraverse f g) x
    bitraverse Identity Identity x `shouldBe` Identity x
    (Compose . fmap (bitraverse h i) . bitraverse f g) x
      `shouldBe` bitraverse (Compose . fmap h . f) (Compose . fmap i . g) x

prop_BitraversableEx :: (Bitraversable p,
                        Eq   (p Char Char), Eq   (p [Char] [Char]), Eq   (p [Int] [Int]),
                        Show (p Char Char), Show (p [Char] [Char]), Show (p [Int] [Int]))
                        => p [Int] [Int] -> Expectation
prop_BitraversableEx = prop_BitraversableLaws
    (replicate 2 . map (chr . abs))
    (replicate 4 . map (chr . abs))
    (++ "hello")
    (++ "world")
    reverse

-------------------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "OneTwoCompose Maybe Either [Int] [Int]" $ do
        prop "satisfies the Bifunctor laws"
            (prop_BifunctorEx     :: OneTwoCompose Maybe Either [Int] [Int] -> Expectation)
        prop "satisfies the Bifoldable laws"
            (prop_BifoldableEx    :: OneTwoCompose Maybe Either [Int] [Int] -> Expectation)
        prop "satisfies the Bitraversable laws"
            (prop_BitraversableEx :: OneTwoCompose Maybe Either [Int] [Int] -> Expectation)
#if MIN_VERSION_template_haskell(2,7,0)
    describe "OneTwoComposeFam Maybe Either [Int] [Int]" $ do
        prop "satisfies the Bifunctor laws"
            (prop_BifunctorEx     :: OneTwoComposeFam Maybe Either [Int] [Int] -> Expectation)
        prop "satisfies the Bifoldable laws"
            (prop_BifoldableEx    :: OneTwoComposeFam Maybe Either [Int] [Int] -> Expectation)
        prop "satisfies the Bitraversable laws"
            (prop_BitraversableEx :: OneTwoComposeFam Maybe Either [Int] [Int] -> Expectation)
#endif
