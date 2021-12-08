-- Reduced from the tagged package

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveGeneric #-}
-- manual generics instances are not safe
{-# LANGUAGE Safe #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
----------------------------------------------------------------------------
-- |
-- Module     : Data.Tagged
-- Copyright  : 2009-2015 Edward Kmett
-- License    : BSD3
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-------------------------------------------------------------------------------

module Data.Tagged
    (
    -- * Tagged values
      Tagged(..)
    , retag
    , untag
    ) where

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData(..))
import Data.Foldable (Foldable(..))
import Control.Monad (liftM)
import Data.Bifunctor
import Data.Bifoldable (Bifoldable(..))
import Data.Bitraversable (Bitraversable(..))
import Data.Data
import Data.Ix (Ix(..))
import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import GHC.Generics (Generic)
import GHC.Generics (Generic1)

-- | A @'Tagged' s b@ value is a value @b@ with an attached phantom type @s@.
-- This can be used in place of the more traditional but less safe idiom of
-- passing in an undefined value with the type, because unlike an @(s -> b)@,
-- a @'Tagged' s b@ can't try to use the argument @s@ as a real value.
--
-- Moreover, you don't have to rely on the compiler to inline away the extra
-- argument, because the newtype is \"free\"
--
-- 'Tagged' has kind @k -> * -> *@ if the compiler supports @PolyKinds@, therefore
-- there is an extra @k@ showing in the instance haddocks that may cause confusion.
newtype Tagged s b = Tagged { unTagged :: b } deriving
  ( Eq, Ord, Ix, Bounded
  , Generic
  , Generic1
  , Typeable
  )

instance (Data s, Data b) => Data (Tagged s b) where
  gfoldl f z (Tagged b) = z Tagged `f` b
  toConstr _ = taggedConstr
  gunfold k z c = case constrIndex c of
    1 -> k (z Tagged)
    _ -> error "gunfold"
  dataTypeOf _ = taggedDataType
  dataCast1 f = gcast1 f
  dataCast2 f = gcast2 f

taggedConstr :: Constr
taggedConstr = mkConstr taggedDataType "Tagged" [] Prefix
{-# INLINE taggedConstr #-}

taggedDataType :: DataType
taggedDataType = mkDataType "Data.Tagged.Tagged" [taggedConstr]
{-# INLINE taggedDataType #-}

instance Show b => Show (Tagged s b) where
    showsPrec n (Tagged b) = showParen (n > 10) $
        showString "Tagged " .
        showsPrec 11 b

instance Read b => Read (Tagged s b) where
    readsPrec d = readParen (d > 10) $ \r ->
        [(Tagged a, t) | ("Tagged", s) <- lex r, (a, t) <- readsPrec 11 s]

instance Semigroup a => Semigroup (Tagged s a) where
    Tagged a <> Tagged b = Tagged (a <> b)
    stimes n (Tagged a)  = Tagged (stimes n a)

instance (Semigroup a, Monoid a) => Monoid (Tagged s a) where
    mempty = Tagged mempty
    mappend = (<>)

instance Functor (Tagged s) where
    fmap f (Tagged x) = Tagged (f x)
    {-# INLINE fmap #-}

-- this instance is provided by the bifunctors package for GHC<7.9
instance Bifunctor Tagged where
    bimap _ g (Tagged b) = Tagged (g b)
    {-# INLINE bimap #-}

-- these instances are provided by the bifunctors package for GHC<8.1
instance Bifoldable Tagged where
    bifoldMap _ g (Tagged b) = g b
    {-# INLINE bifoldMap #-}

instance Bitraversable Tagged where
    bitraverse _ g (Tagged b) = Tagged <$> g b
    {-# INLINE bitraverse #-}

instance NFData b => NFData (Tagged s b) where
    rnf (Tagged b) = rnf b

instance Applicative (Tagged s) where
    pure = Tagged
    {-# INLINE pure #-}
    Tagged f <*> Tagged x = Tagged (f x)
    {-# INLINE (<*>) #-}
    _ *> n = n
    {-# INLINE (*>) #-}

instance Monad (Tagged s) where
    return = pure
    {-# INLINE return #-}
    Tagged m >>= k = k m
    {-# INLINE (>>=) #-}
    (>>) = (*>)
    {-# INLINE (>>) #-}

instance Foldable (Tagged s) where
    foldMap f (Tagged x) = f x
    {-# INLINE foldMap #-}
    fold (Tagged x) = x
    {-# INLINE fold #-}
    foldr f z (Tagged x) = f x z
    {-# INLINE foldr #-}
    foldl f z (Tagged x) = f z x
    {-# INLINE foldl #-}
    foldl1 _ (Tagged x) = x
    {-# INLINE foldl1 #-}
    foldr1 _ (Tagged x) = x
    {-# INLINE foldr1 #-}

instance Traversable (Tagged s) where
    traverse f (Tagged x) = Tagged <$> f x
    {-# INLINE traverse #-}
    sequenceA (Tagged x) = Tagged <$> x
    {-# INLINE sequenceA #-}
    mapM f (Tagged x) = liftM Tagged (f x)
    {-# INLINE mapM #-}
    sequence (Tagged x) = liftM Tagged x
    {-# INLINE sequence #-}

instance Enum a => Enum (Tagged s a) where
    succ = fmap succ
    pred = fmap pred
    toEnum = Tagged . toEnum
    fromEnum (Tagged x) = fromEnum x
    enumFrom (Tagged x) = map Tagged (enumFrom x)
    enumFromThen (Tagged x) (Tagged y) = map Tagged (enumFromThen x y)
    enumFromTo (Tagged x) (Tagged y) = map Tagged (enumFromTo x y)
    enumFromThenTo (Tagged x) (Tagged y) (Tagged z) =
        map Tagged (enumFromThenTo x y z)

instance Num a => Num (Tagged s a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = Tagged . fromInteger

instance Real a => Real (Tagged s a) where
    toRational (Tagged x) = toRational x

instance IsString a => IsString (Tagged s a) where
    fromString = Tagged . fromString

-- | Some times you need to change the tag you have lying around.
-- Idiomatic usage is to make a new combinator for the relationship between the
-- tags that you want to enforce, and define that combinator using 'retag'.
--
-- @
-- data Succ n
-- retagSucc :: 'Tagged' n a -> 'Tagged' (Succ n) a
-- retagSucc = 'retag'
-- @
retag :: Tagged s b -> Tagged t b
retag = Tagged . unTagged
{-# INLINE retag #-}

-- | Alias for 'unTagged'
untag :: Tagged s b -> b
untag = unTagged
