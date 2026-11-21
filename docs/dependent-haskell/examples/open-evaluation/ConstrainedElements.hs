{-# LANGUAGE ConstraintKinds, DataKinds, ExperimentalDependentTypes, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, Modifiers, PolyKinds, RankNTypes, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module ConstrainedElements (Cursor(..), PrintableCursor, Collection(..), Allowed, Collects(..), FieldKey, Field(..), field, applyField, withField, KnownIndex(..), Reflection, reflectMaybe, MaybeBoolEcho, AtNothing, FormatKey, RenderFormat(..), Formatted, FieldViews(..), ViewsFor) where

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

-- | Identify the domain of a field format.
data FormatKey (c :: Type)

-- | Select the field representation for a known domain and a detail flag.
class RenderFormat c where
    type FieldView c (detailed :: Bool) :: Type

-- | The detailed view adds a label to the raw value.
type family Formatted c (detailed :: Bool) where
    Formatted c 'False = c
    Formatted c 'True = (String, c)

instance {-# OVERLAPPABLE #-} RenderFormat (FormatKey c) where
    type FieldView (FormatKey c) detailed = Formatted c detailed

-- | Keep the raw and detailed presentations of a field.
data FieldViews (view :: Bool %U -> Type) = FieldViews (view 'False) (view 'True)

-- | Choose the format domain and leave the detail flag open.
type ViewsFor c = FieldViews (FieldView (FormatKey c))

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

-- | Advance past the first element when it satisfies the predicate.
class Collection c e => Cursor c e | c -> e where
    type Remaining c
    type Remaining c = Rest c
    advanceIf :: (e -> Bool) -> c -> Maybe (Remaining c)

-- | A cursor whose remaining collection can be printed.
type PrintableCursor c e = (Cursor c e, Show (Remaining c))

instance Cursor [a] a where
    advanceIf p (x:xs) | p x = Just xs
    advanceIf _ _ = Nothing

instance Cursor (Maybe a) a where
    type Remaining (Maybe a) = Rest (Maybe a)
    advanceIf p (Just x) | p x = Just ()
    advanceIf _ _ = Nothing
