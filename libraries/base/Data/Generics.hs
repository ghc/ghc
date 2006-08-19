-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Data.Generics.Basics)
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell 
-- See <http://www.cs.vu.nl/boilerplate/>. To scrap your boilerplate it
-- is sufficient to import the present module, which simply re-exports all
-- themes of the Data.Generics library.
--
-----------------------------------------------------------------------------

module Data.Generics ( 

  -- * All Data.Generics modules
  module Data.Generics.Basics,	  -- primitives
  module Data.Generics.Instances, -- instances of Data class
  module Data.Generics.Aliases,	  -- aliases for type case, generic types
  module Data.Generics.Schemes,	  -- traversal schemes (everywhere etc.)
  module Data.Generics.Text,	  -- generic read and show
  module Data.Generics.Twins,  	  -- twin traversal, e.g., generic eq

#ifndef __HADDOCK__
	-- Data types for the sum-of-products type encoding;
        -- included for backwards compatibility; maybe obsolete.
	(:*:)(..), (:+:)(..), Unit(..)
#endif

 ) where

------------------------------------------------------------------------------

import Prelude	-- So that 'make depend' works

#ifdef __GLASGOW_HASKELL__
#ifndef __HADDOCK__
	-- Data types for the sum-of-products type encoding;
        -- included for backwards compatibility; maybe obsolete.
import GHC.Base ( (:*:)(..), (:+:)(..), Unit(..) )
#endif
#endif

import Data.Generics.Basics
import Data.Generics.Instances
import Data.Generics.Aliases
import Data.Generics.Schemes
import Data.Generics.Text
import Data.Generics.Twins
