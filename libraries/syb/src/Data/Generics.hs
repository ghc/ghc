{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2004
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Data.Generics.Basics)
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell 
-- See <http://www.cs.uu.nl/wiki/GenericProgramming/SYB>. To scrap your
-- boilerplate it is sufficient to import the present module, which simply
-- re-exports all themes of the Data.Generics library.
--
-----------------------------------------------------------------------------

module Data.Generics (

  -- * All Data.Generics modules
  module Data.Data,               -- primitives and instances of the Data class
  module Data.Generics.Aliases,   -- aliases for type case, generic types
  module Data.Generics.Schemes,   -- traversal schemes (everywhere etc.)
  module Data.Generics.Text,      -- generic read and show
  module Data.Generics.Twins,     -- twin traversal, e.g., generic eq
  module Data.Generics.Builders,  -- term builders

 ) where

------------------------------------------------------------------------------

import Data.Data
import Data.Generics.Instances ()
import Data.Generics.Aliases
import Data.Generics.Schemes
import Data.Generics.Text
import Data.Generics.Twins
import Data.Generics.Builders
