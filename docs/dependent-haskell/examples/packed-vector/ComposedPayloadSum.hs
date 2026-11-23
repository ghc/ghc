{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
module ComposedPayloadSum where

import Data.Proxy (Proxy(..))
import GHC.Sigma (Sigma(..))

class First key where
    type FirstBody key (tag :: Bool)

instance {-# OVERLAPPABLE #-} First [a] where
    type FirstBody [a] False = Int
    type FirstBody [a] True = Char

class Second key where
    type SecondBody key (tag :: Bool)

instance {-# OVERLAPPABLE #-} Second [a] where
    type SecondBody [a] False = String
    type SecondBody [a] True = Bool

type Optional = some (tag :: Bool) | Maybe (FirstBody [Int] tag)
type Repeated = some (tag :: Bool) | (FirstBody [Int] tag, FirstBody [Int] tag)
type Distinct = some (tag :: Bool) | (FirstBody [Int] tag, SecondBody [Int] tag)

#ifdef WRONG_OWNER
class First key => Both key where
#else
class (First key, Second key) => Both key where
#endif
    type Scoped key

instance Both [Int] where
    type Scoped [Int] = some (tag :: Bool) | (FirstBody [Int] tag, SecondBody [Int] tag)

scopeValue :: Scoped [Int]
scopeValue = Pair True ('x', True)

distinctValue :: Distinct
distinctValue = Pair False (42, "body")

repeatedValue :: Repeated
repeatedValue = Pair True ('x', 'y')

payloadSchema :: Sigma Bool payload -> Proxy payload
payloadSchema _ = Proxy

#ifdef WITNESS_DOMAIN
class ByTag (tag :: Bool) where
    type Tagged tag
instance ByTag False where type Tagged False = Int
instance ByTag True where type Tagged True = Char
type InvalidWitness = some (tag :: Bool) | Maybe (Tagged tag)
#endif

#ifdef UNRESOLVED_DOMAIN
type InvalidDomain key = some (tag :: Bool) | Maybe (FirstBody key tag)
#endif

#ifdef OMITTED_OWNER_PARAMETER
class Omitted key other | key -> other where
    type OmittedBody key (tag :: Bool)
instance Omitted Bool Int where
    type OmittedBody Bool False = Int
    type OmittedBody Bool True = Char
type InvalidOmitted = some (tag :: Bool) | Maybe (OmittedBody Bool tag)
#endif
