-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Shortcuts
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

module Data.Generics.Shortcuts ( 

	-- * Cut-off traversal
	everywhere1RT'

  ) where

-----------------------------------------------------------------------------


import Data.Generics.Basics
import Data.Generics.Aliases
import Data.Generics.Types
import Data.Types


-----------------------------------------------------------------------------


-- Run-time cut-off for top-down traversal with one specific type case.
-- This is only for illustrative purposes. 
-- The naive approach here is prohibitively inefficient.
-- 
everywhere1RT' :: (Data a, Data b) => (a -> a) -> b -> b
everywhere1RT' f t =
  if not $ typeReachableFrom (argType f) (typeValOf t)
   then t
   else gmapT (everywhere1RT' f) (mkT f t)
