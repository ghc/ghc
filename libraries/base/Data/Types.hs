-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Types
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a style of encoding types as values and using
-- them. This style is seens as an alternative to the pragmatic style
-- used in Data.Typeable and elsewhere, i.e., simply use an undefined
-- to denote a type argument. This pragmatic style suffers from lack
-- of robustness: one fells tempted to pattern match on undefineds.
--
-----------------------------------------------------------------------------

module Data.Types
  (

	-- * Types as values
	TypeVal,		-- view type "a" as "a -> ()"
	typeVal,		-- :: TypeVal a
	typeValOf,		-- :: a -> TypeVal a
	undefinedType,		-- :: TypeVal a -> a
	withType,		-- :: a -> TypeVal a -> a
	argType,		-- :: (a -> b) -> TypeVal a
	resType,		-- :: (a -> b) -> TypeVal b
	TypeFun			-- functions on types

  ) where


-------------------------------------------------------------
--
--	Types as values
--
-------------------------------------------------------------


-- Type as values to stipulate use of undefineds
type TypeVal a = a -> ()


--- The value that denotes a type
typeVal :: TypeVal a
typeVal = const ()


-- Map a value to its type
typeValOf :: a -> TypeVal a
typeValOf _ = typeVal


-- Stipulate this idiom!
undefinedType :: TypeVal a -> a
undefinedType _ = undefined


-- Constrain a type
withType :: a -> TypeVal a -> a
withType x _ = x


-- The argument type of a function
argType :: (a -> b) -> TypeVal a
argType _ = typeVal


-- The result type of a function
resType :: (a -> b) -> TypeVal b
resType _ = typeVal


-- Type functions,
-- i.e., functions mapping types to values
--
type TypeFun a r = TypeVal a -> r
