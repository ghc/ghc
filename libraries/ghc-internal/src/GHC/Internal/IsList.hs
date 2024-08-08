{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.IsList
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- @since base-4.17.0.0
-----------------------------------------------------------------------------

module GHC.Internal.IsList
  ( IsList(..)
  ) where

import GHC.Internal.Base
import GHC.Internal.Functor.ZipList (ZipList(..))
import GHC.Internal.Stack
import GHC.Internal.Data.Version ( Version(..), makeVersion )

-- | The 'IsList' class and its methods are intended to be used in
--   conjunction with the OverloadedLists extension.
--
-- @since base-4.7.0.0
class IsList l where
  -- | The 'Item' type function returns the type of items of the structure
  --   @l@.
  type Item l

  -- | The 'fromList' function constructs the structure @l@ from the given
  --   list of @Item l@
  fromList  :: [Item l] -> l

  -- | The 'fromListN' function takes the input list's length and potentially
  --   uses it to construct the structure @l@ more efficiently compared to
  --   'fromList'. If the given number does not equal to the input list's length
  --   the behaviour of 'fromListN' is not specified.
  --
  --   prop> fromListN (length xs) xs == fromList xs
  fromListN :: Int -> [Item l] -> l
  fromListN _ = fromList

  -- | The 'toList' function extracts a list of @Item l@ from the structure @l@.
  --   It should satisfy fromList . toList = id.
  toList :: l -> [Item l]

-- | @since base-4.7.0.0
instance IsList [a] where
  type (Item [a]) = a
  fromList = id
  toList = id

-- | @since base-4.15.0.0
instance IsList (ZipList a) where
  type Item (ZipList a) = a
  fromList = ZipList
  toList = getZipList

-- | @since base-4.9.0.0
instance IsList (NonEmpty a) where
  type Item (NonEmpty a) = a

  fromList (a:as) = a :| as
  fromList [] = errorWithoutStackTrace "NonEmpty.fromList: empty list"

  toList (a :| as) = a : as

-- | @since base-4.8.0.0
instance IsList Version where
  type (Item Version) = Int
  fromList = makeVersion
  toList = versionBranch

-- | Be aware that 'fromList . toList = id' only for unfrozen 'CallStack's,
-- since 'toList' removes frozenness information.
--
-- @since base-4.9.0.0
instance IsList CallStack where
  type (Item CallStack) = (String, SrcLoc)
  fromList = fromCallSiteList
  toList   = getCallStack
