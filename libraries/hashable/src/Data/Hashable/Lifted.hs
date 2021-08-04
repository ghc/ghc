{-# LANGUAGE Safe #-}

------------------------------------------------------------------------
-- |
-- Module      :  Data.Hashable.Lifted
-- Copyright   :  (c) Milan Straka 2010
--                (c) Johan Tibell 2011
--                (c) Bryan O'Sullivan 2011, 2012
-- SPDX-License-Identifier : BSD-3-Clause
-- Maintainer  :  johan.tibell@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Lifting of the 'Hashable' class to unary and binary type constructors.
-- These classes are needed to express the constraints on arguments of
-- types that are parameterized by type constructors. Fixed-point data
-- types and monad transformers are such types.

module Data.Hashable.Lifted
    ( -- * Type Classes
      Hashable1(..)
    , Hashable2(..)
      -- * Auxiliary Functions
    , hashWithSalt1
    , hashWithSalt2
    , defaultLiftHashWithSalt
      -- * Motivation
      -- $motivation
    ) where

import Data.Hashable.Class

-- $motivation
--
-- This type classes provided in this module are used to express constraints
-- on type constructors in a Haskell98-compatible fashion. As an example, consider
-- the following two types (Note that these instances are not actually provided
-- because @hashable@ does not have @transformers@ or @free@ as a dependency):
--
-- > newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }
-- > data Free f a = Pure a | Free (f (Free f a))
--
-- The 'Hashable1' instances for @WriterT@ and @Free@ could be written as:
--
-- > instance (Hashable w, Hashable1 m) => Hashable1 (WriterT w m) where
-- >     liftHashWithSalt h s (WriterT m) =
-- >         liftHashWithSalt (liftHashWithSalt2 h hashWithSalt) s m
-- > instance Hashable1 f => Hashable1 (Free f) where
-- >     liftHashWithSalt h = go where
-- >         go s x = case x of
-- >             Pure a -> h s a
-- >             Free p -> liftHashWithSalt go s p
--
-- The 'Hashable' instances for these types can be trivially recovered with
-- 'hashWithSalt1':
--
-- > instance (Hashable w, Hashable1 m, Hashable a) => Hashable (WriterT w m a) where
-- >     hashWithSalt = hashWithSalt1
-- > instance (Hashable1 f, Hashable a) => Hashable (Free f a) where
-- >     hashWithSalt = hashWithSalt1

--
-- $discussion
--
-- Regardless of whether 'hashWithSalt1' is used to provide an implementation
-- of 'hashWithSalt', they should produce the same hash when called with
-- the same arguments. This is the only law that 'Hashable1' and 'Hashable2'
-- are expected to follow.
--
-- The typeclasses in this module only provide lifting for 'hashWithSalt', not
-- for 'hash'. This is because such liftings cannot be defined in a way that
-- would satisfy the @liftHash@ variant of the above law. As an illustration
-- of the problem we run into, let us assume that 'Hashable1' were
-- given a 'liftHash' method:
--
-- > class Hashable1 t where
-- >     liftHash :: (a -> Int) -> t a -> Int
-- >     liftHashWithSalt :: (Int -> a -> Int) -> Int -> t a -> Int
--
-- Even for a type as simple as 'Maybe', the problem manifests itself. The
-- 'Hashable' instance for 'Maybe' is:
--
-- > distinguisher :: Int
-- > distinguisher = ...
-- >
-- > instance Hashable a => Hashable (Maybe a) where
-- >     hash Nothing = 0
-- >     hash (Just a) = distinguisher `hashWithSalt` a
-- >     hashWithSalt s Nothing = ...
-- >     hashWithSalt s (Just a) = ...
--
-- The implementation of 'hash' calls 'hashWithSalt' on @a@. The hypothetical
-- @liftHash@ defined earlier only accepts an argument that corresponds to
-- the implementation of 'hash' for @a@. Consequently, this formulation of
-- @liftHash@ would not provide a way to match the current behavior of 'hash'
-- for 'Maybe'. This problem gets worse when 'Either' and @[]@ are considered.
-- The solution adopted in this library is to omit @liftHash@ entirely.

