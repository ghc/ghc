-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Counts
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

module Data.Generics.Counts ( 

	-- * Generic operations for counting terms
	glength,
	gcount,
	gnodecount,
	gtypecount

 ) where

------------------------------------------------------------------------------


import Data.Generics.Basics
import Data.Generics.Aliases
import Data.Generics.Schemes


------------------------------------------------------------------------------
--
--	Generic operations for counting terms
--
------------------------------------------------------------------------------


-- | Count the number of immediate subterms of the given term
glength :: GenericQ Int
glength = length . gmapL (const ())


-- | Determine the number of all suitable nodes in a given term
gcount :: GenericQ Bool -> GenericQ Int
gcount p =  everything (+) (\x -> if p x then 1 else 0)


-- | Determine the number of all nodes in a given term
gnodecount :: GenericQ Int
gnodecount = gcount (const True)


-- | Determine the number of nodes of a given type in a given term
gtypecount :: Typeable a => (a -> ()) -> GenericQ Int
gtypecount f = gcount (False `mkQ` (const True . f))
