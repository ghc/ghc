{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Either
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The Either type, and associated operations.
--
-----------------------------------------------------------------------------

module Data.Either (
   Either(..),
   either,
   lefts,
   rights,
   isLeft,
   isRight,
   partitionEithers,
 ) where

import GHC.Base
import GHC.Show
import GHC.Read

import Data.Type.Equality

-- $setup
-- Allow the use of some Prelude functions in doctests.
-- >>> import Prelude ( (+), (*), length, putStrLn )

{-
-- just for testing
import Test.QuickCheck
-}

{-|

The 'Either' type represents values with two possibilities: a value of
type @'Either' a b@ is either @'Left' a@ or @'Right' b@.

The 'Either' type is sometimes used to represent a value which is
either correct or an error; by convention, the 'Left' constructor is
used to hold an error value and the 'Right' constructor is used to
hold a correct value (mnemonic: \"right\" also means \"correct\").

==== __Examples__

The type @'Either' 'String' 'Int'@ is the type of values which can be either
a 'String' or an 'Int'. The 'Left' constructor can be used only on
'String's, and the 'Right' constructor can be used only on 'Int's:

>>> let s = Left "foo" :: Either String Int
>>> s
Left "foo"
>>> let n = Right 3 :: Either String Int
>>> n
Right 3
>>> :type s
s :: Either String Int
>>> :type n
n :: Either String Int

The 'fmap' from our 'Functor' instance will ignore 'Left' values, but
will apply the supplied function to values contained in a 'Right':

>>> let s = Left "foo" :: Either String Int
>>> let n = Right 3 :: Either String Int
>>> fmap (*2) s
Left "foo"
>>> fmap (*2) n
Right 6

The 'Monad' instance for 'Either' allows us to chain together multiple
actions which may fail, and fail overall if any of the individual
steps failed. First we'll write a function that can either parse an
'Int' from a 'Char', or fail.

>>> import Data.Char ( digitToInt, isDigit )
>>> :{
    let parseEither :: Char -> Either String Int
        parseEither c
          | isDigit c = Right (digitToInt c)
          | otherwise = Left "parse error"
>>> :}

The following should work, since both @\'1\'@ and @\'2\'@ can be
parsed as 'Int's.

>>> :{
    let parseMultiple :: Either String Int
        parseMultiple = do
          x <- parseEither '1'
          y <- parseEither '2'
          return (x + y)
>>> :}

>>> parseMultiple
Right 3

But the following should fail overall, since the first operation where
we attempt to parse @\'m\'@ as an 'Int' will fail:

>>> :{
    let parseMultiple :: Either String Int
        parseMultiple = do
          x <- parseEither 'm'
          y <- parseEither '2'
          return (x + y)
>>> :}

>>> parseMultiple
Left "parse error"

-}
data  Either a b  =  Left a | Right b
  deriving (Eq, Ord, Read, Show)

instance Functor (Either a) where
    fmap _ (Left x) = Left x
    fmap f (Right y) = Right (f y)

instance Applicative (Either e) where
    pure          = Right
    Left  e <*> _ = Left e
    Right f <*> r = fmap f r

instance Monad (Either e) where
    Left  l >>= _ = Left l
    Right r >>= k = k r

-- | Case analysis for the 'Either' type.
-- If the value is @'Left' a@, apply the first function to @a@;
-- if it is @'Right' b@, apply the second function to @b@.
--
-- ==== __Examples__
--
-- We create two values of type @'Either' 'String' 'Int'@, one using the
-- 'Left' constructor and another using the 'Right' constructor. Then
-- we apply \"either\" the 'length' function (if we have a 'String')
-- or the \"times-two\" function (if we have an 'Int'):
--
-- >>> let s = Left "foo" :: Either String Int
-- >>> let n = Right 3 :: Either String Int
-- >>> either length (*2) s
-- 3
-- >>> either length (*2) n
-- 6
--
either                  :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x)     =  f x
either _ g (Right y)    =  g y


-- | Extracts from a list of 'Either' all the 'Left' elements.
-- All the 'Left' elements are extracted in order.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> let list = [ Left "foo", Right 3, Left "bar", Right 7, Left "baz" ]
-- >>> lefts list
-- ["foo","bar","baz"]
--
lefts   :: [Either a b] -> [a]
lefts x = [a | Left a <- x]

-- | Extracts from a list of 'Either' all the 'Right' elements.
-- All the 'Right' elements are extracted in order.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> let list = [ Left "foo", Right 3, Left "bar", Right 7, Left "baz" ]
-- >>> rights list
-- [3,7]
--
rights   :: [Either a b] -> [b]
rights x = [a | Right a <- x]

-- | Partitions a list of 'Either' into two lists.
-- All the 'Left' elements are extracted, in order, to the first
-- component of the output.  Similarly the 'Right' elements are extracted
-- to the second component of the output.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> let list = [ Left "foo", Right 3, Left "bar", Right 7, Left "baz" ]
-- >>> partitionEithers list
-- (["foo","bar","baz"],[3,7])
--
-- The pair returned by @'partitionEithers' x@ should be the same
-- pair as @('lefts' x, 'rights' x)@:
--
-- >>> let list = [ Left "foo", Right 3, Left "bar", Right 7, Left "baz" ]
-- >>> partitionEithers list == (lefts list, rights list)
-- True
--
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers = foldr (either left right) ([],[])
 where
  left  a ~(l, r) = (a:l, r)
  right a ~(l, r) = (l, a:r)

-- | Return `True` if the given value is a `Left`-value, `False` otherwise.
--
-- @since 4.7.0.0
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> isLeft (Left "foo")
-- True
-- >>> isLeft (Right 3)
-- False
--
-- Assuming a 'Left' value signifies some sort of error, we can use
-- 'isLeft' to write a very simple error-reporting function that does
-- absolutely nothing in the case of success, and outputs \"ERROR\" if
-- any error occurred.
--
-- This example shows how 'isLeft' might be used to avoid pattern
-- matching when one does not care about the value contained in the
-- constructor:
--
-- >>> import Control.Monad ( when )
-- >>> let report e = when (isLeft e) $ putStrLn "ERROR"
-- >>> report (Right 1)
-- >>> report (Left "parse error")
-- ERROR
--
isLeft :: Either a b -> Bool
isLeft (Left  _) = True
isLeft (Right _) = False

-- | Return `True` if the given value is a `Right`-value, `False` otherwise.
--
-- @since 4.7.0.0
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> isRight (Left "foo")
-- False
-- >>> isRight (Right 3)
-- True
--
-- Assuming a 'Left' value signifies some sort of error, we can use
-- 'isRight' to write a very simple reporting function that only
-- outputs \"SUCCESS\" when a computation has succeeded.
--
-- This example shows how 'isRight' might be used to avoid pattern
-- matching when one does not care about the value contained in the
-- constructor:
--
-- >>> import Control.Monad ( when )
-- >>> let report e = when (isRight e) $ putStrLn "SUCCESS"
-- >>> report (Left "parse error")
-- >>> report (Right 1)
-- SUCCESS
--
isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True

-- instance for the == Boolean type-level equality operator
type family EqEither a b where
  EqEither ('Left x)  ('Left y)  = x == y
  EqEither ('Right x) ('Right y) = x == y
  EqEither a          b          = 'False
type instance a == b = EqEither a b

{-
{--------------------------------------------------------------------
  Testing
--------------------------------------------------------------------}
prop_partitionEithers :: [Either Int Int] -> Bool
prop_partitionEithers x =
  partitionEithers x == (lefts x, rights x)
-}

