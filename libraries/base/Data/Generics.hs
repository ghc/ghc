-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- "Scrap your boilerplate" --- Generic programming in Haskell 
-- See <http://www.cs.vu.nl/boilerplate/>.
--
-----------------------------------------------------------------------------

module Data.Generics ( 

	-- * Re-export all relevant modules
	module Data.Generics.Basics,
	module Data.Generics.Aliases,
	module Data.Generics.Schemes,
	module Data.Generics.Twins,
	module Data.Generics.Strings,
	module Data.Generics.Counts,
	module Data.Generics.Types

#ifndef __HADDOCK__
	,
	-- Data types for the sum-of-products type encoding;
        -- included for backwards compatibility; maybe obsolete
	(:*:)(..), (:+:)(..), Unit(..)
#endif

 ) where

------------------------------------------------------------------------------

import Prelude	-- So that 'make depend' works

#ifdef __GLASGOW_HASKELL__
#ifndef __HADDOCK__
import GHC.Base ( (:*:)(..), (:+:)(..), Unit(..) )
#endif
#endif

import Data.Generics.Basics
import Data.Generics.Aliases
import Data.Generics.Schemes
import Data.Generics.Twins
import Data.Generics.Strings
import Data.Generics.Counts
import Data.Generics.Types
