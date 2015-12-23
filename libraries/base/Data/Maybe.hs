{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Maybe
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The Maybe type, and associated operations.
--
-----------------------------------------------------------------------------

module Data.Maybe
   (
     Maybe(Nothing,Just)

   , maybe

   , isJust
   , isNothing
   , fromJust
   , fromMaybe
   , listToMaybe
   , maybeToList
   , catMaybes
   , mapMaybe
   ) where

import GHC.Base

-- $setup
-- Allow the use of some Prelude functions in doctests.
-- >>> import Prelude ( (*), odd, show, sum )

-- ---------------------------------------------------------------------------
-- Functions over Maybe

-- | The 'maybe' function takes a default value, a function, and a 'Maybe'
-- value.  If the 'Maybe' value is 'Nothing', the function returns the
-- default value.  Otherwise, it applies the function to the value inside
-- the 'Just' and returns the result.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> maybe False odd (Just 3)
-- True
--
-- >>> maybe False odd Nothing
-- False
--
-- Read an integer from a string using 'readMaybe'. If we succeed,
-- return twice the integer; that is, apply @(*2)@ to it. If instead
-- we fail to parse an integer, return @0@ by default:
--
-- >>> import Text.Read ( readMaybe )
-- >>> maybe 0 (*2) (readMaybe "5")
-- 10
-- >>> maybe 0 (*2) (readMaybe "")
-- 0
--
-- Apply 'show' to a @Maybe Int@. If we have @Just n@, we want to show
-- the underlying 'Int' @n@. But if we have 'Nothing', we return the
-- empty string instead of (for example) \"Nothing\":
--
-- >>> maybe "" show (Just 5)
-- "5"
-- >>> maybe "" show Nothing
-- ""
--
maybe :: b -> (a -> b) -> Maybe a -> b
maybe n _ Nothing  = n
maybe _ f (Just x) = f x

-- | The 'isJust' function returns 'True' iff its argument is of the
-- form @Just _@.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> isJust (Just 3)
-- True
--
-- >>> isJust (Just ())
-- True
--
-- >>> isJust Nothing
-- False
--
-- Only the outer constructor is taken into consideration:
--
-- >>> isJust (Just Nothing)
-- True
--
isJust         :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

-- | The 'isNothing' function returns 'True' iff its argument is 'Nothing'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> isNothing (Just 3)
-- False
--
-- >>> isNothing (Just ())
-- False
--
-- >>> isNothing Nothing
-- True
--
-- Only the outer constructor is taken into consideration:
--
-- >>> isNothing (Just Nothing)
-- False
--
isNothing         :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

-- | The 'fromJust' function extracts the element out of a 'Just' and
-- throws an error if its argument is 'Nothing'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> fromJust (Just 1)
-- 1
--
-- >>> 2 * (fromJust (Just 10))
-- 20
--
-- >>> 2 * (fromJust Nothing)
-- *** Exception: Maybe.fromJust: Nothing
--
fromJust          :: Maybe a -> a
fromJust Nothing  = errorWithoutStackTrace "Maybe.fromJust: Nothing" -- yuck
fromJust (Just x) = x

-- | The 'fromMaybe' function takes a default value and and 'Maybe'
-- value.  If the 'Maybe' is 'Nothing', it returns the default values;
-- otherwise, it returns the value contained in the 'Maybe'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> fromMaybe "" (Just "Hello, World!")
-- "Hello, World!"
--
-- >>> fromMaybe "" Nothing
-- ""
--
-- Read an integer from a string using 'readMaybe'. If we fail to
-- parse an integer, we want to return @0@ by default:
--
-- >>> import Text.Read ( readMaybe )
-- >>> fromMaybe 0 (readMaybe "5")
-- 5
-- >>> fromMaybe 0 (readMaybe "")
-- 0
--
fromMaybe     :: a -> Maybe a -> a
fromMaybe d x = case x of {Nothing -> d;Just v  -> v}

-- | The 'maybeToList' function returns an empty list when given
-- 'Nothing' or a singleton list when not given 'Nothing'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> maybeToList (Just 7)
-- [7]
--
-- >>> maybeToList Nothing
-- []
--
-- One can use 'maybeToList' to avoid pattern matching when combined
-- with a function that (safely) works on lists:
--
-- >>> import Text.Read ( readMaybe )
-- >>> sum $ maybeToList (readMaybe "3")
-- 3
-- >>> sum $ maybeToList (readMaybe "")
-- 0
--
maybeToList            :: Maybe a -> [a]
maybeToList  Nothing   = []
maybeToList  (Just x)  = [x]

-- | The 'listToMaybe' function returns 'Nothing' on an empty list
-- or @'Just' a@ where @a@ is the first element of the list.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> listToMaybe []
-- Nothing
--
-- >>> listToMaybe [9]
-- Just 9
--
-- >>> listToMaybe [1,2,3]
-- Just 1
--
-- Composing 'maybeToList' with 'listToMaybe' should be the identity
-- on singleton/empty lists:
--
-- >>> maybeToList $ listToMaybe [5]
-- [5]
-- >>> maybeToList $ listToMaybe []
-- []
--
-- But not on lists with more than one element:
--
-- >>> maybeToList $ listToMaybe [1,2,3]
-- [1]
--
listToMaybe           :: [a] -> Maybe a
listToMaybe []        =  Nothing
listToMaybe (a:_)     =  Just a

-- | The 'catMaybes' function takes a list of 'Maybe's and returns
-- a list of all the 'Just' values.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> catMaybes [Just 1, Nothing, Just 3]
-- [1,3]
--
-- When constructing a list of 'Maybe' values, 'catMaybes' can be used
-- to return all of the \"success\" results (if the list is the result
-- of a 'map', then 'mapMaybe' would be more appropriate):
--
-- >>> import Text.Read ( readMaybe )
-- >>> [readMaybe x :: Maybe Int | x <- ["1", "Foo", "3"] ]
-- [Just 1,Nothing,Just 3]
-- >>> catMaybes $ [readMaybe x :: Maybe Int | x <- ["1", "Foo", "3"] ]
-- [1,3]
--
catMaybes              :: [Maybe a] -> [a]
catMaybes ls = [x | Just x <- ls]

-- | The 'mapMaybe' function is a version of 'map' which can throw
-- out elements.  In particular, the functional argument returns
-- something of type @'Maybe' b@.  If this is 'Nothing', no element
-- is added on to the result list.  If it is @'Just' b@, then @b@ is
-- included in the result list.
--
-- ==== __Examples__
--
-- Using @'mapMaybe' f x@ is a shortcut for @'catMaybes' $ 'map' f x@
-- in most cases:
--
-- >>> import Text.Read ( readMaybe )
-- >>> let readMaybeInt = readMaybe :: String -> Maybe Int
-- >>> mapMaybe readMaybeInt ["1", "Foo", "3"]
-- [1,3]
-- >>> catMaybes $ map readMaybeInt ["1", "Foo", "3"]
-- [1,3]
--
-- If we map the 'Just' constructor, the entire list should be returned:
--
-- >>> mapMaybe Just [1,2,3]
-- [1,2,3]
--
mapMaybe          :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ []     = []
mapMaybe f (x:xs) =
 let rs = mapMaybe f xs in
 case f x of
  Nothing -> rs
  Just r  -> r:rs
{-# NOINLINE [1] mapMaybe #-}

{-# RULES
"mapMaybe"     [~1] forall f xs. mapMaybe f xs
                    = build (\c n -> foldr (mapMaybeFB c f) n xs)
"mapMaybeList" [1]  forall f. foldr (mapMaybeFB (:) f) [] = mapMaybe f
  #-}

{-# NOINLINE [0] mapMaybeFB #-}
mapMaybeFB :: (b -> r -> r) -> (a -> Maybe b) -> a -> r -> r
mapMaybeFB cons f x next = case f x of
  Nothing -> next
  Just r -> cons r next
