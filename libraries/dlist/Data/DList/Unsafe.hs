{- ORMOLU_DISABLE -}
-- Options passed to Haddock
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

-- CPP: GHC >= 7.8 for Safe Haskell
#if __GLASGOW_HASKELL__ >= 708
{-

The 'Data.DList.Unsafe' module exports 'UnsafeDList' and 'unsafeApplyDList',
which allow breaking the invariant of the 'DList' newtype. Therefore, we
explicitly mark 'Data.DList.Unsafe' as unsafe.

-}
{-# LANGUAGE Unsafe #-}
#endif

-----------------------------------------------------------------------------

{-|

Module: Data.DList.Unsafe
Copyright: © 2006-2009 Don Stewart, 2013-2020 Sean Leather
License: BSD-3-Clause

Maintainer: sean.leather@gmail.com
Stability: stable

This module exports the 'DList' constructor, 'UnsafeDList', and the record label,
'unsafeApplyDList', both of which can be used to create unsafe 'DList' values
that break the invariant preserved by the names exported from 'Data.DList'.

-}
{- ORMOLU_ENABLE -}

module Data.DList.Unsafe (DList (UnsafeDList, unsafeApplyDList)) where

import Data.DList.Internal
