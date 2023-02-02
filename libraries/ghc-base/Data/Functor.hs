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
--
-- A type @f@ is a Functor if it provides a function @fmap@ which, given any types @a@ and @b@,
-- lets you apply any function of type @(a -> b)@ to turn an @f a@ into an @f b@, preserving the
-- structure of @f@.
--
-- ==== __Examples__
--
--  >>> fmap show (Just 1)  --  (a   -> b)      -> f a       -> f b
--  Just "1"                --  (Int -> String) -> Maybe Int -> Maybe String
--
--  >>> fmap show Nothing   --  (a   -> b)      -> f a       -> f b
--  Nothing                 --  (Int -> String) -> Maybe Int -> Maybe String
--
--  >>> fmap show [1,2,3]   --  (a   -> b)      -> f a       -> f b
--  ["1","2","3"]           --  (Int -> String) -> [Int]     -> [String]
--
--  >>> fmap show []        --  (a   -> b)      -> f a       -> f b
--  []                      --  (Int -> String) -> [Int]     -> [String]
--
-- The 'fmap' function is also available as the infix operator '<$>':
--
--  >>> fmap show (Just 1) --  (Int -> String) -> Maybe Int -> Maybe String
--  Just "1"
--  >>> show <$> (Just 1)  --  (Int -> String) -> Maybe Int -> Maybe String
--  Just "1"

module Data.Functor
    (
      Functor(..),
      ($>),
      (<$>),
      (<&>),
      void,
    ) where

import GHC.Base ( Functor(..), flip )

-- $setup
-- Allow the use of Prelude in doctests.
-- >>> import Prelude hiding ((<$>))

infixl 4 <$>

-- | An infix synonym for 'fmap'.
--
-- The name of this operator is an allusion to 'Prelude.$'.
-- Note the similarities between their types:
--
-- >  ($)  ::              (a -> b) ->   a ->   b
-- > (<$>) :: Functor f => (a -> b) -> f a -> f b
--
-- Whereas 'Prelude.$' is function application, '<$>' is function
-- application lifted over a 'Functor'.
--
-- ==== __Examples__
--
-- Convert from a @'Data.Maybe.Maybe' 'Data.Int.Int'@ to a @'Data.Maybe.Maybe'
-- 'Data.String.String'@ using 'Prelude.show':
--
-- >>> show <$> Nothing
-- Nothing
-- >>> show <$> Just 3
-- Just "3"
--
-- Convert from an @'Data.Either.Either' 'Data.Int.Int' 'Data.Int.Int'@ to an
-- @'Data.Either.Either' 'Data.Int.Int'@ 'Data.String.String' using 'Prelude.show':
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
-- Apply 'Prelude.even' to the second element of a pair:
--
-- >>> even <$> (2,2)
-- (2,True)
--
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

infixl 1 <&>

-- | Flipped version of '<$>'.
--
-- @
-- ('<&>') = 'flip' 'fmap'
-- @
--
-- @since 4.11.0.0
--
-- ==== __Examples__
-- Apply @(+1)@ to a list, a 'Data.Maybe.Just' and a 'Data.Either.Right':
--
-- >>> Just 2 <&> (+1)
-- Just 3
--
-- >>> [1,2,3] <&> (+1)
-- [2,3,4]
--
-- >>> Right 3 <&> (+1)
-- Right 4
--
(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as

infixl 4 $>

-- | Flipped version of '<$'.
--
-- @since 4.7.0.0
--
-- ==== __Examples__
--
-- Replace the contents of a @'Data.Maybe.Maybe' 'Data.Int.Int'@ with a constant
-- 'Data.String.String':
--
-- >>> Nothing $> "foo"
-- Nothing
-- >>> Just 90210 $> "foo"
-- Just "foo"
--
-- Replace the contents of an @'Data.Either.Either' 'Data.Int.Int' 'Data.Int.Int'@
-- with a constant 'Data.String.String', resulting in an @'Data.Either.Either'
-- 'Data.Int.Int' 'Data.String.String'@:
--
-- >>> Left 8675309 $> "foo"
-- Left 8675309
-- >>> Right 8675309 $> "foo"
-- Right "foo"
--
-- Replace each element of a list with a constant 'Data.String.String':
--
-- >>> [1,2,3] $> "foo"
-- ["foo","foo","foo"]
--
-- Replace the second element of a pair with a constant 'Data.String.String':
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
-- Replace the contents of a @'Data.Maybe.Maybe' 'Data.Int.Int'@ with unit:
--
-- >>> void Nothing
-- Nothing
-- >>> void (Just 3)
-- Just ()
--
-- Replace the contents of an @'Data.Either.Either' 'Data.Int.Int' 'Data.Int.Int'@
-- with unit, resulting in an @'Data.Either.Either' 'Data.Int.Int' '()'@:
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
