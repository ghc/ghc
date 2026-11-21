{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, FlexibleInstances, GADTs, TypeFamilies, UndecidableInstances #-}

module NativeConstrainedSumProvider (Choice (Shape), Permit (Permit), Staged (Staged), make) where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import GHC.Sigma (Sigma (Pair))

class Allowed (a :: Type)
instance Allowed Bool

data Permit a where
    Permit :: Allowed a => Permit a

class Packet (a :: Type) where
    type Packed a

#ifdef USE_NAMED
type PackedAlias a = some (tag :: Bool) | (Proxy tag, Proxy a)
#endif

instance Allowed a => Packet [a] where
#ifdef USE_NAMED
    type Packed [a] = PackedAlias a
#else
    type Packed [a] = some (tag :: Bool) | (Proxy tag, Proxy a)
#endif

class Packet a => Forward (a :: Type) where
    type Forwarded a

instance Allowed a => Forward [a] where
    type Forwarded [a] = Packed [a]

make :: Allowed a => Proxy a -> Forwarded [a]
make _ = Pair True (error "unused forwarded payload")

type Private = some (tag :: Bool) | Proxy tag
newtype Wrapped = Wrapped Private

class Choice (a :: Type) where
    type Shape a (b :: Type)

data Staged a where
#ifdef EARLY_DOMAIN
    Staged :: Shape a Char -> (a ~ Bool) => Proxy a -> Staged a
#else
    Staged :: Proxy a -> (a ~ Bool) => Shape a Char -> Staged a
#endif

instance Choice Bool where
    type Shape Bool Char = Private
    type Shape Bool Int = Wrapped
