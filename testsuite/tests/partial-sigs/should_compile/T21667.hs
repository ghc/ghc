{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}

module T21667 where

import GHC.TypeLits
import Data.Kind
import Data.Functor.Identity
import Data.Functor.Const
import Data.Functor


-- import fluff
type ASetter s t a b = (a -> Identity b) -> s -> Identity t
type Getting r s a = (a -> Const r a) -> s -> Const r s
type Lens s t a b = forall f . Functor f => (a -> f b) -> (s -> f t)
type Traversal s t a b = forall g . Applicative g => (a -> g b) -> (s -> g t)

set :: ASetter s t a b -> b -> s -> t
set = undefined

view :: MonadReader s m => Getting a s a -> m a
view = undefined

class Monad m => MonadReader r (m :: Type -> Type) | m -> r where
instance MonadReader r ((->) r) where


-- test case

data Item (a :: Type) (f :: Symbol -> Type -> Type)

l :: Lens (Item a f) (Item a' g) (f "1" ()) (g "1" ())
l = undefined

type ExoticTraversal' a y f = Traversal
          (Item a f)
          (Item a f)
          (f y ())
          (f y ())

test :: forall a f. ExoticTraversal' a _ f
-- The point here is that we must skolemise all the forall'd
-- variables of test at once; but some are visible (a,b), and
-- some are hidden (the forall g. hidden under Traversal).
-- That led to #21667
test f x = f (view l x) <&> \w -> set l w x

{- A variety of isomorphic signatures for test

test :: Traversal
          (Item a f)
          (Item a f)
          (f _ ())
          (f _ ())

test :: forall a f. forall g . Applicative g
                    => (f _ () -> g (f _ ()))
                    -> (Item a f -> g (Item a f))

test :: forall a f. forall g . Applicative g
                    => (f "1" () -> g (f "1" ()))
                    -> (Item a f -> g (Item a f))

test :: forall a f g . Applicative g
                    => (f _ () -> g (f _ ()))
                    -> (Item a f -> g (Item a f))
-}


{- The error reported in #21667

    • Couldn't match type ‘a0’ with ‘a’
        arising from a functional dependency between:
          constraint ‘MonadReader (Item a0 f) ((->) (Item a f))’
            arising from a use of ‘view’
          instance ‘MonadReader r ((->) r)’ at T21667.hs:29:10-31
    • ‘a0’ is untouchable
        inside the constraints: Applicative g
        bound by a type expected by the context:
                   forall (g :: Type -> Type).
                   Applicative g =>
                   (f "1" () -> g (f "1" ())) -> Item a f -> g (Item a f)
        at T21667.hs:53:1-43
      ‘a’ is a rigid type variable bound by
        the inferred type of
          test :: forall (g1 :: Type -> Type).
                  Applicative g1 =>
                  (f "1" () -> g1 (f "1" ())) -> Item a f -> g1 (Item a f)
        at T21667.hs:50:16
-}