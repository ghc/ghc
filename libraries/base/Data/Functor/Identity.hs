{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveTraversable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Identity
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The identity functor and monad.
--
-- This trivial type constructor serves two purposes:
--
-- * It can be used with functions parameterized by functor or monad classes.
--
-- * It can be used as a base monad to which a series of monad
--   transformers may be applied to construct a composite monad.
--   Most monad transformer modules include the special case of
--   applying the transformer to 'Identity'.  For example, @State s@
--   is an abbreviation for @StateT s 'Identity'@.
--
-- @since 4.8.0.0
-----------------------------------------------------------------------------

module Data.Functor.Identity (
    Identity(..),
  ) where

import Control.Monad.Fix
import Data.Coerce
import Data.Foldable

-- | Identity functor and monad. (a non-strict monad)
--
-- @since 4.8.0.0
newtype Identity a = Identity { runIdentity :: a }
    deriving (Eq, Ord, Traversable)

-- | This instance would be equivalent to the derived instances of the
-- 'Identity' newtype if the 'runIdentity' field were removed
instance (Read a) => Read (Identity a) where
    readsPrec d = readParen (d > 10) $ \ r ->
        [(Identity x,t) | ("Identity",s) <- lex r, (x,t) <- readsPrec 11 s]

-- | This instance would be equivalent to the derived instances of the
-- 'Identity' newtype if the 'runIdentity' field were removed
instance (Show a) => Show (Identity a) where
    showsPrec d (Identity x) = showParen (d > 10) $
        showString "Identity " . showsPrec 11 x

-- ---------------------------------------------------------------------------
-- Identity instances for Functor and Monad

instance Foldable Identity where
    foldMap                = coerce

    elem                   = (. runIdentity) #. (==)
    foldl                  = coerce
    foldl'                 = coerce
    foldl1 _               = runIdentity
    foldr f z (Identity x) = f x z
    foldr'                 = foldr
    foldr1 _               = runIdentity
    length _               = 1
    maximum                = runIdentity
    minimum                = runIdentity
    null _                 = False
    product                = runIdentity
    sum                    = runIdentity
    toList (Identity x)    = [x]

instance Functor Identity where
    fmap     = coerce

instance Applicative Identity where
    pure     = Identity
    (<*>)    = coerce

instance Monad Identity where
    return   = Identity
    m >>= k  = k (runIdentity m)

instance MonadFix Identity where
    mfix f   = Identity (fix (runIdentity . f))


-- | Internal (non-exported) 'Coercible' helper for 'elem'
--
-- See Note [Function coercion] in "Data.Foldable" for more details.
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
(#.) _f = coerce
