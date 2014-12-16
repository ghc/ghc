{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functors: uniform action over a parameterized type, generalizing the
-- 'Data.List.map' function on lists.

module Data.Functor
    (
      Functor(fmap),
      (<$),
      ($>),
      (<$>),
      void,
    ) where

import GHC.Base ( Functor(..), flip )

-- $setup
-- Allow the use of Prelude in doctests.
-- >>> import Prelude

infixl 4 <$>

-- | An infix synonym for 'fmap'.
--
-- ==== __Examples__
--
-- Convert from a @'Maybe' 'Int'@ to a @'Maybe' 'String'@ using 'show':
--
-- >>> show <$> Nothing
-- Nothing
-- >>> show <$> Just 3
-- Just "3"
--
-- Convert from an @'Either' 'Int' 'Int'@ to an @'Either' 'Int'@
-- 'String' using 'show':
--
-- >>> show <$> Left 17
-- Left 17
-- >>> show <$> Right 17
-- Right "17"
--
-- Double each element of a list:
--
-- >>> (*2) <$> [1,2,3]
-- [2,4,6]
--
-- Apply 'even' to the second element of a pair:
--
-- >>> even <$> (2,2)
-- (2,True)
--
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

infixl 4 $>

-- | Flipped version of '<$'.
--
-- @since 4.7.0.0
--
-- ==== __Examples__
--
-- Replace the contents of a @'Maybe' 'Int'@ with a constant 'String':
--
-- >>> Nothing $> "foo"
-- Nothing
-- >>> Just 90210 $> "foo"
-- Just "foo"
--
-- Replace the contents of an @'Either' 'Int' 'Int'@ with a constant
-- 'String', resulting in an @'Either' 'Int' 'String'@:
--
-- >>> Left 8675309 $> "foo"
-- Left 8675309
-- >>> Right 8675309 $> "foo"
-- Right "foo"
--
-- Replace each element of a list with a constant 'String':
--
-- >>> [1,2,3] $> "foo"
-- ["foo","foo","foo"]
--
-- Replace the second element of a pair with a constant 'String':
--
-- >>> (1,2) $> "foo"
-- (1,"foo")
--
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

-- | @'void' value@ discards or ignores the result of evaluation, such
-- as the return value of an 'System.IO.IO' action.
--
-- ==== __Examples__
--
-- Replace the contents of a @'Maybe' 'Int'@ with unit:
--
-- >>> void Nothing
-- Nothing
-- >>> void (Just 3)
-- Just ()
--
-- Replace the contents of an @'Either' 'Int' 'Int'@ with unit,
-- resulting in an @'Either' 'Int' '()'@:
--
-- >>> void (Left 8675309)
-- Left 8675309
-- >>> void (Right 8675309)
-- Right ()
--
-- Replace every element of a list with unit:
--
-- >>> void [1,2,3]
-- [(),(),()]
--
-- Replace the second element of a pair with unit:
--
-- >>> void (1,2)
-- (1,())
--
-- Discard the result of an 'System.IO.IO' action:
--
-- >>> mapM print [1,2]
-- 1
-- 2
-- [(),()]
-- >>> void $ mapM print [1,2]
-- 1
-- 2
--
void :: Functor f => f a -> f ()
void x = () <$ x
