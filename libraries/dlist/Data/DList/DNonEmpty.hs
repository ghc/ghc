{- ORMOLU_DISABLE -}
{-# LANGUAGE CPP #-}

-- CPP: GHC >= 7.8 for Safe Haskell
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE Safe #-}
#endif

-----------------------------------------------------------------------------

-- CPP: Ignore unused imports when Haddock is run
#if defined(__HADDOCK_VERSION__)
{-# OPTIONS_GHC -Wno-unused-imports #-}
#endif

-----------------------------------------------------------------------------

{-|

Module: Data.DList.DNonEmpty
Copyright: © 2017-2020 Oleg Grenrus, 2020 Sean Leather
License: BSD-3-Clause

Maintainer: sean.leather@gmail.com
Stability: stable

A __non-empty difference list__ is a difference list paired with a 'head'
element. Like the difference list, it supports&#x00A0;\(\mathcal{O}\)(@1@)
'append' and 'snoc' operations.

This module provides the type for a non-empty difference list, 'DNonEmpty', and
a collection of supporting functions for (a) converting to and from 'NonEmpty'
and 'DList' and (b) operating efficiently on 'DNonEmpty' values. The functions
also retain the non-strict semantics of 'NonEmpty'.

-}
{- ORMOLU_ENABLE -}

module Data.DList.DNonEmpty
  ( -- * Non-Empty Difference List Type
    DNonEmpty((:|)),

    -- * Conversion
    fromNonEmpty,
    toNonEmpty,
    toList,
    fromList,

    -- * Basic Functions
    singleton,
    cons,
    snoc,
    append,
    head,
    tail,
    unfoldr,
    map,
  )
where

-----------------------------------------------------------------------------

import Data.DList.DNonEmpty.Internal

-- CPP: Import only for Haddock
#if defined(__HADDOCK_VERSION__)
import Data.List.NonEmpty (NonEmpty)
import Data.DList (DList)
#endif

{- ORMOLU_DISABLE -}
{-

The 'Data.DList.DNonEmpty' module exists only to export names from
'Data.DList.DNonEmpty.Internal'. Some names conflict with 'Prelude', so we hide
all imports from 'Prelude'.

-}
{- ORMOLU_ENABLE -}
import Prelude ()
