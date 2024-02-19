{-# LANGUAGE Safe #-}

-- |
--
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
--
--  >>> show <$> (Just 1)  --  (Int -> String) -> Maybe Int -> Maybe String
--  Just "1"

module Data.Functor
    (Functor(..),
     ($>),
     (<$>),
     (<&>),
     unzip,
     void
     ) where

import GHC.Internal.Data.Functor