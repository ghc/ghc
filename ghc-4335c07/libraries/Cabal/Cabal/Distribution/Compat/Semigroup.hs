{-# LANGUAGE CPP                         #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE TypeOperators               #-}

-- | Compatibility layer for "Data.Semigroup"
module Distribution.Compat.Semigroup
    ( Semigroup((<>))
    , Mon.Monoid(..)
    , All(..)
    , Any(..)

    , Last'(..)

    , gmappend
    , gmempty
    ) where

import Distribution.Compat.Binary (Binary)

import Control.Applicative as App
import GHC.Generics
#if __GLASGOW_HASKELL__ >= 711
-- Data.Semigroup is available since GHC 8.0/base-4.9
import Data.Semigroup
import qualified Data.Monoid as Mon
#else
-- provide internal simplified non-exposed class for older GHCs
import Data.Monoid as Mon (Monoid(..), All(..), Any(..), Dual(..))
-- containers
import Data.Set (Set)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.IntMap (IntMap)


class Semigroup a where
    (<>) :: a -> a -> a

-- several primitive instances
instance Semigroup () where
    _ <> _ = ()

instance Semigroup [a] where
    (<>) = (++)

instance Semigroup a => Semigroup (Dual a) where
    Dual a <> Dual b = Dual (b <> a)

instance Semigroup a => Semigroup (Maybe a) where
    Nothing <> b       = b
    a       <> Nothing = a
    Just a  <> Just b  = Just (a <> b)

instance Semigroup (Either a b) where
    Left _ <> b = b
    a      <> _ = a

instance Semigroup Ordering where
    LT <> _ = LT
    EQ <> y = y
    GT <> _ = GT

instance Semigroup b => Semigroup (a -> b) where
    f <> g = \a -> f a <> g a

instance Semigroup All where
    All a <> All b = All (a && b)

instance Semigroup Any where
    Any a <> Any b = Any (a || b)

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
    (a,b) <> (a',b') = (a<>a',b<>b')

instance (Semigroup a, Semigroup b, Semigroup c)
         => Semigroup (a, b, c) where
    (a,b,c) <> (a',b',c') = (a<>a',b<>b',c<>c')

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
         => Semigroup (a, b, c, d) where
    (a,b,c,d) <> (a',b',c',d') = (a<>a',b<>b',c<>c',d<>d')

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e)
         => Semigroup (a, b, c, d, e) where
    (a,b,c,d,e) <> (a',b',c',d',e') = (a<>a',b<>b',c<>c',d<>d',e<>e')

-- containers instances
instance Semigroup IntSet where
  (<>) = mappend

instance Ord a => Semigroup (Set a) where
  (<>) = mappend

instance Semigroup (IntMap v) where
  (<>) = mappend

instance Ord k => Semigroup (Map k v) where
  (<>) = mappend
#endif

-- | Cabal's own 'Data.Monoid.Last' copy to avoid requiring an orphan
-- 'Binary' instance.
--
-- Once the oldest `binary` version we support provides a 'Binary'
-- instance for 'Data.Monoid.Last' we can remove this one here.
--
-- NB: 'Data.Semigroup.Last' is defined differently and not a 'Monoid'
newtype Last' a = Last' { getLast' :: Maybe a }
                deriving (Eq, Ord, Read, Show, Binary,
                          Functor, App.Applicative, Generic)

instance Semigroup (Last' a) where
    x <> Last' Nothing = x
    _ <> x             = x

instance Monoid (Last' a) where
    mempty = Last' Nothing
    mappend = (<>)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Stolen from Edward Kmett's BSD3-licensed `semigroups` package

-- | Generically generate a 'Semigroup' ('<>') operation for any type
-- implementing 'Generic'. This operation will append two values
-- by point-wise appending their component fields. It is only defined
-- for product types.
--
-- @
-- 'gmappend' a ('gmappend' b c) = 'gmappend' ('gmappend' a b) c
-- @
gmappend :: (Generic a, GSemigroup (Rep a)) => a -> a -> a
gmappend x y = to (gmappend' (from x) (from y))

class GSemigroup f where
    gmappend' :: f p -> f p -> f p

instance Semigroup a => GSemigroup (K1 i a) where
    gmappend' (K1 x) (K1 y) = K1 (x <> y)

instance GSemigroup f => GSemigroup (M1 i c f) where
    gmappend' (M1 x) (M1 y) = M1 (gmappend' x y)

instance (GSemigroup f, GSemigroup g) => GSemigroup (f :*: g) where
    gmappend' (x1 :*: x2) (y1 :*: y2) = gmappend' x1 y1 :*: gmappend' x2 y2

-- | Generically generate a 'Monoid' 'mempty' for any product-like type
-- implementing 'Generic'.
--
-- It is only defined for product types.
--
-- @
-- 'gmappend' 'gmempty' a = a = 'gmappend' a 'gmempty'
-- @

gmempty :: (Generic a, GMonoid (Rep a)) => a
gmempty = to gmempty'

class GSemigroup f => GMonoid f where
    gmempty' :: f p

instance (Semigroup a, Monoid a) => GMonoid (K1 i a) where
    gmempty' = K1 mempty

instance GMonoid f => GMonoid (M1 i c f) where
    gmempty' = M1 gmempty'

instance (GMonoid f, GMonoid g) => GMonoid (f :*: g) where
    gmempty' = gmempty' :*: gmempty'
