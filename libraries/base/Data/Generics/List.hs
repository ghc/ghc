-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.List
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell 
-- See <http://www.cs.vu.nl/boilerplate/>. The present module illustrates
-- one possible treatment of polymorphic datatypes for specialising
-- generic functions.
--
-----------------------------------------------------------------------------

module Data.Generics.List ( 

	-- * Processing polymorphic lists
	isList,
	isNil,
	isCons,
	lgmapQ


 ) where


------------------------------------------------------------------------------

#ifdef __HADDOCK__
import Prelude
#endif
import Data.Maybe
import Data.Generics.Basics

-------------------------------------------------------------
--
--	Processing polymorphic lists
--
-------------------------------------------------------------


-- | Test for list datatype
isList :: Data a => a -> Bool
isList x = typerepTyCon (typeOf x) ==
           typerepTyCon (typeOf (undefined::[()]))


-- | Test for nil
isNil :: Data a => a -> Bool
isNil x = toConstr x == toConstr ([]::[()])


-- | Test for cons
isCons :: Data a => a -> Bool
isCons x = toConstr x == toConstr (():[])


-- | gmapQ for polymorphic lists; Nothing for other than lists
lgmapQ :: forall a q. Data a => (forall a. Data a => a -> q) -> a -> Maybe [q]
lgmapQ f x =
  if not $ isList x 
   then Nothing
   else Just ( if isNil x
                 then []
                 else if isCons x
                   then ( gmapQi 0 f x : gmapQi 1 (fromJust . lgmapQ f) x )
                   else error "lgmapQ"
             )
