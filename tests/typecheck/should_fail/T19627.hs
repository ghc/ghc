{-# language BlockArguments #-}
{-# language DefaultSignatures #-}
{-# language DerivingStrategies #-}
{-# language EmptyCase #-}
{-# language ExplicitNamespaces #-}
{-# language ImportQualifiedPost #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language LinearTypes #-}
{-# language NoStarIsType #-}
{-# language PolyKinds #-}
{-# language QuantifiedConstraints #-}
{-# language RankNTypes #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language StandaloneKindSignatures #-}
{-# language StrictData #-}
{-# language TupleSections #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeFamilyDependencies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module T19627 where

import Data.Kind
import Prelude hiding ( Functor(..) )

--------------------

class (Prop (Not p), Not (Not p) ~ p) => Prop (p :: Type) where
  type Not p :: Type
  (!=) :: p -> Not p -> r

data Y (a :: Type) (b :: Type) (c :: Type) where
  L :: Y a b a
  R :: Y a b b

newtype a & b = With (forall c. Y a b c -> c)

with :: (forall c. Y a b c -> c) -> a & b
with = With

runWith :: a & b -> Y a b c -> c
runWith (With f) = f

withL' :: a & b -> a
withL' (With f) = f L

withR' :: a & b -> b
withR' (With f) = f R

instance (Prop a, Prop b) => Prop (a & b) where
  type Not (a & b) = Not a `Either` Not b
  w != Left a  = withL' w != a
  w != Right b = withR' w != b

instance (Prop a, Prop b) => Prop (Either a b) where
  type Not (Either a b) = Not a & Not b
  Left a  != w = a != withL' w
  Right a != w = a != withR' w

newtype Yoneda f a = Yoneda
  (forall r. Prop r => (a -> r) -> f r)

data Noneda f a where
  Noneda :: Prop r => !(f r <#- (a ⊸ r)) -> Noneda f a

liftYoneda :: forall f a i. (Functor f, Prop a, Iso i) => i (f a) (Yoneda f a)
liftYoneda = iso \case
  L -> lowerYoneda'
  R -> lol \case
    L -> \(Noneda ((a2r :: a ⊸ r) :-#> nfr)) -> runLol (fmap @f @a @r a2r) L nfr
    R -> \fa -> Yoneda do
      lol \case
        R -> \f -> fmap' f fa
        L -> \nfr -> whyNot \a2r -> fmap a2r fa != nfr


type family NotApart (p :: Type -> Type -> Type) :: Type -> Type -> Type

class
  ( forall a b. (Prop a, Prop b) => Prop (p a b)
  , NotApart (NotIso p) ~ p
  ) => Iso p where
  type NotIso p = (q :: Type -> Type -> Type) | q -> p
  iso :: (forall c. Y (b ⊸ a) (a ⊸ b) c -> c) -> p a b

data b <#- a where (:-#>) :: a -> Not b -> b <#- a
newtype a ⊸ b = Lol (forall c. Y (Not b %1 -> Not a) (a %1 -> b) c -> c)

class
  ( forall a. Prop a => Prop (f a)
  ) => Functor f where
  fmap' :: (Prop a, Prop b, Lol l, Lol l') => l ((a ⊸ b)) (l' (f a) (f b))

fmap :: forall f a b l. (Functor f, Prop a, Prop b, Lol l) => (a ⊸ b) -> l (f a) (f b)
fmap f = fmap' f

class Iso p => Lol (p :: Type -> Type -> Type) where
  lol :: (forall c. Y (Not b -> Not a) (a -> b) c -> c) -> p a b
  apartR :: Not (p a b) -> b <#- a
