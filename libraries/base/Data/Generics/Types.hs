-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Types
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

module Data.Generics.Types ( 

	-- * Generic operations to reify types
	constrArity,
	typeReachableFrom,

 ) where


------------------------------------------------------------------------------


import Data.Types
import Data.Generics.Basics
import Data.Generics.Aliases
import Data.Generics.Counts



-- Generic type functions,
-- i.e., functions mapping types to values
--
type GTypeFun r  = forall a. Typeable a => TypeFun a r



------------------------------------------------------------------------------
--
--	Compute arity of a constructor against a type argument
--
------------------------------------------------------------------------------


constrArity :: Data a => (a -> ()) -> Constr -> Int
constrArity ta c = glength $ withType (fromConstr c) ta


------------------------------------------------------------------------------
--
--	Reachability relation on types
--
------------------------------------------------------------------------------

--
-- Test if nodes of type "a" are reachable from nodes of type "b".
-- This is a naive, inefficient encoding.
-- As of writing, it does not even cope with recursive types.
--
typeReachableFrom :: (Data a, Data b) => TypeVal a -> TypeVal b -> Bool
typeReachableFrom (a::TypeVal a) (b::TypeVal b) =
  or ( sameType a b
     : map (recurse . fromConstr) (dataTypeCons $ dataTypeOf b)
     )
  where

    -- See if a is reachable from immediate subterms of a kind of b 
    recurse :: b -> Bool
    recurse = or
            . gmapL ( typeReachableFrom a 
                    . typeValOf
                    )
