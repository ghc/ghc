{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, PatternSynonyms #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Tuple
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/ghc-prim/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (GHC extensions)
--
-- The tuple data types
--
-- Users should not import this module.  It is GHC internal only.
--
-----------------------------------------------------------------------------

module GHC.Tuple (
  module GHC.Tuple.Prim,
  Solo (Solo, MkSolo),
) where

import GHC.CString ()  -- Make sure we do it first, so that the
                       -- implicit Typeable stuff can see GHC.Types.TyCon
                       -- and unpackCString# etc
import GHC.Tuple.Prim

default () -- Double and Integer aren't available yet

{-# DEPRECATED data Solo "The Solo constructor has been renamed to MkSolo to avoid punning." #-}
pattern Solo :: a -> Solo a
pattern Solo x = MkSolo x
{-# COMPLETE Solo #-}
