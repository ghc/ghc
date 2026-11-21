{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, TypeFamilies, UndecidableSuperClasses #-}
module NativeConstrainedFamilyProvider (Collects(..), Display, Allowed, Defaulted(..), Pair(..), Key, KeyElement, Output(..), Projected, Chain(..), Fixed(..), ListBacking, ListSource(..)) where

import Data.Kind (Constraint)

class Allowed a
instance Allowed Bool

class Collects c where
    type Elem c

-- | The two component dictionaries of a displayable collection.
type Display c = (Show (Elem c), Collects c)

instance Allowed a => Collects [a] where
    type Elem [a] = a

class Defaulted a where
    type Def a
    type Def a = Char

instance Defaulted Int

class Pair a b where
    type Swapped b a

instance Pair Bool Int where
    type Swapped Int Bool = Char

data Key a

instance {-# OVERLAPPABLE #-} Collects (Key a) where
    type Elem (Key a) = Bool

-- | Select a domain that an importing module can specialize.
type KeyElement a = Elem (Key a)

class Output c e | c -> e where
    type Project c
    type Explicit c e

instance {-# OVERLAPPABLE #-} Output (Key a) a where
    type Project (Key a) = Bool
    type Explicit (Key a) a = Bool

-- | Carry an omitted owner parameter through an imported alias.
type Projected a = Project (Key a)

class Chain a b c | a -> b, b -> c where
    type Target a

instance Chain Bool Char Int where
    type Target Bool = Int

class (e ~ Maybe a) => Fixed a e where
    type FromFixed a

instance Fixed Bool (Maybe Bool) where
    type FromFixed Bool = Char

-- | Constraints supplied by a collection to its users.
type family ListBacking c :: Constraint
type instance ListBacking [a] = ()

-- | Select the element type and read the first element.
class ListBacking c => ListSource c where
    type SourceItem c
    first :: c -> Maybe (SourceItem c)

instance Allowed a => ListSource [a] where
    type SourceItem [a] = a
    first [] = Nothing
    first (x:_) = Just x
