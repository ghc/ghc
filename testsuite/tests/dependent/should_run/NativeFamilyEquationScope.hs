{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, QuantifiedConstraints, TypeFamilies, UndecidableInstances, UndecidableSuperClasses #-}
module NativeFamilyEquationScope where

import Data.Coerce (Coercible)
import Data.Kind (Type)
import NativeConstrainedFamilyProvider (Allowed, Collects(..), Key, KeyElement, Output(..))

type Forget a = Bool

class Collects c => View c where
    type Viewed c
    type Viewed c = Elem c

instance Allowed a => View [a]

class View c => Layer c where
    type Wrapped c

instance Allowed a => Layer [a] where
    type Wrapped [a] = Maybe (Elem [a])

-- An inferred owner argument of Project must match the one supplied by Via.
class Output c e => Via c e | c -> e where
    type Hidden c
#ifdef WRONG_OWNER
    type Hidden c = Explicit c Bool
#else
    type Hidden c = Project c
#endif

#if defined(WRITER_ONLY)
class SourceOnly c where
    type Copy c
    type Copy c = KeyElement c
#elif defined(WRONG_ARGUMENT)
class Collects a => Witness (a :: Type) (b :: Type) where
    type Sibling a b
    type Sibling a b = Forget (Elem b)
#elif defined(REPRESENTATIONAL)
class (Coercible a b, Collects b) => Equal (a :: Type) (b :: Type) where
#else
class (a ~ b, Collects b) => Equal (a :: Type) (b :: Type) where
#endif
#if !defined(WRITER_ONLY) && !defined(WRONG_ARGUMENT)
    type Rewritten a b
    type Rewritten a b = Elem a
#endif

#if defined(MISSING_PREMISE)
class (forall x. Allowed x => Collects (f x)) => Quantified (f :: Type -> Type) (a :: Type) where
#else
class (Allowed a, forall x. Allowed x => Collects (f x)) => Quantified (f :: Type -> Type) (a :: Type) where
#endif
    type Projected f a
    type Projected f a = Elem (f a)
