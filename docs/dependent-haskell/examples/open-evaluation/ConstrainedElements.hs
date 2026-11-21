{-# LANGUAGE DataKinds, ExperimentalDependentTypes, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, Modifiers, PolyKinds, RankNTypes, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}
module ConstrainedElements (Collection(..), Allowed, Collects(..), FieldKey, Field(..), field, applyField, withField, KnownIndex(..), Reflection, reflectMaybe, MaybeBoolEcho, AtNothing) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (U)

class Allowed a
instance Allowed Bool

class Collects c where
    type Elem c

instance Allowed a => Collects [a] where
    type Elem [a] = a

-- | Select the element domain for a runtime flag.
data FieldKey (tag :: Bool)

instance Collects (FieldKey 'True) where
    type Elem (FieldKey 'True) = Char

-- | Store a field with its renderer and its domain evidence.
data Field (tag :: Bool) where
    Field :: Collects (FieldKey tag)
          => Elem (FieldKey tag) -> (Elem (FieldKey tag) -> String) -> Field tag

-- | Supply a field only for a flag with a domain instance.
field :: foreach (tag :: Bool) -> Maybe (Field tag)
field False = Nothing
field True = Just (Field 'x' (: []))

-- | Use the evidence for this tag to form the element type.
applyField :: foreach (tag :: Bool) -> Collects (FieldKey tag)
           => (Elem (FieldKey tag) -> String) -> Elem (FieldKey tag) -> String
applyField _ render value = render value

-- | Open a field and pass its renderer and element to a polymorphic consumer.
withField :: (forall (c :: Type). Collects c => Proxy c -> (Elem c -> String) -> Elem c -> r)
          -> foreach (tag :: Bool) -> Field tag -> r
withField visit tag (Field value render) = visit (Proxy @(FieldKey tag)) render value

-- | Select the associated family by the kind of its argument.
class KnownIndex (k :: Type) where
    type Echo (x :: k) :: k

instance Allowed a => KnownIndex (Maybe a) where
    type Echo x = x

-- | The premise provides the domain of each retained index.
type Reflection a = Allowed a => foreach (x :: Maybe a) -> Echo x :~: x

-- | Pass the family at the kind whose instance is already known.
type MaybeBoolEcho = Echo :: Maybe Bool %U -> Maybe Bool

-- | Apply an unmatchable function to the empty constructor.
type AtNothing (f :: Maybe Bool %U -> Maybe Bool) = f 'Nothing

-- | Reflect a runtime index using the instance for its kind.
reflectMaybe :: forall a. Reflection a
reflectMaybe _ = Refl

-- | Split a collection into one element and the remainder.
class Collection c e | c -> e where
    type Rest c
    uncons :: c -> Maybe (e, Rest c)

instance Collection [a] a where
    type Rest [a] = [a]
    uncons [] = Nothing
    uncons (x:xs) = Just (x, xs)

instance Collection (Maybe a) a where
    type Rest (Maybe a) = ()
    uncons = fmap (\x -> (x, ()))
