{- ORMOLU_DISABLE -}
{-# LANGUAGE CPP #-}

-- CPP: GHC >= 7.8 && <= 8 for 'pattern' required in the export list
#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 800
{-# LANGUAGE PatternSynonyms #-}
#endif

-- CPP: GHC >= 7.8 for Safe Haskell
#if __GLASGOW_HASKELL__ >= 708
{-

The 'Data.DList' module imports the unsafe module 'Data.DList.Internal' but
exports only its safe aspects. Specifically, it does not export the 'DList'
constructor 'UnsafeDList' or record label 'unsafeApplyDList'. Therefore, we mark
'Data.DList' as trustworthy.

-}
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------

{-|

Module: Data.DList
Copyright: © 2006-2009 Don Stewart, 2013-2020 Sean Leather
License: BSD-3-Clause

Maintainer: sean.leather@gmail.com
Stability: stable

A __difference list__ is an abstraction representing a list that
supports&#x00A0;\(\mathcal{O}\)(@1@) 'append' and 'snoc' operations. This module
provides the type for a difference list, 'DList', and a collection of supporting
functions for (a) converting to and from lists and (b) operating on 'DList's
efficiently.

-}
{- ORMOLU_ENABLE -}

module Data.DList
  ( -- * Difference List Type

-- CPP: GHC >= 8 for pattern synonyms allowed in the constructor
#if __GLASGOW_HASKELL__ >= 800
    DList (Nil, Cons),
#else
    DList,

-- CPP: GHC >= 7.8 && <= 8 for 'pattern' required in the export list
#if __GLASGOW_HASKELL__ >= 708
    -- ** Bundled Patterns
    pattern Nil,
    pattern Cons,
#endif

#endif

    -- * Conversion
    fromList,
    toList,
    apply,

    -- * Basic Functions
    empty,
    singleton,
    cons,
    snoc,
    append,
    concat,
    replicate,
    head,
    tail,
    unfoldr,
    foldr,
    map,
    intercalate,
  )
where

-----------------------------------------------------------------------------

import Data.DList.Internal

{- ORMOLU_DISABLE -}
{-

The 'Data.DList' module exists only to export names from 'Data.DList.Internal'.
Some names conflict with 'Prelude', so we hide all imports from 'Prelude'.

-}
{- ORMOLU_ENABLE -}
import Prelude ()
