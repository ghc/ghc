{-# LANGUAGE Safe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Function
-- Copyright   :  Nils Anders Danielsson 2006
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Simple combinators working solely on and with functions.
--
-----------------------------------------------------------------------------

module Data.Function
  ( -- * "Prelude" re-exports
    id, const, (.), flip, ($)
    -- * Other combinators
  , fix
  , on
  ) where

import Prelude

infixl 0 `on`

-- | @'fix' f@ is the least fixed point of the function @f@,
-- i.e. the least defined @x@ such that @f x = x@.
fix :: (a -> a) -> a
fix f = let x = f x in x

-- | @(*) \`on\` f = \\x y -> f x * f y@.
--
-- Typical usage: @'Data.List.sortBy' ('compare' \`on\` 'fst')@.
--
-- Algebraic properties:
--
-- * @(*) \`on\` 'id' = (*)@ (if @(*) &#x2209; {&#x22a5;, 'const' &#x22a5;}@)
--
-- * @((*) \`on\` f) \`on\` g = (*) \`on\` (f . g)@
--
-- * @'flip' on f . 'flip' on g = 'flip' on (g . f)@

-- Proofs (so that I don't have to edit the test-suite):

--   (*) `on` id
-- =
--   \x y -> id x * id y
-- =
--   \x y -> x * y
-- = { If (*) /= _|_ or const _|_. }
--   (*)

--   (*) `on` f `on` g
-- =
--   ((*) `on` f) `on` g
-- =
--   \x y -> ((*) `on` f) (g x) (g y)
-- =
--   \x y -> (\x y -> f x * f y) (g x) (g y)
-- =
--   \x y -> f (g x) * f (g y)
-- =
--   \x y -> (f . g) x * (f . g) y
-- =
--   (*) `on` (f . g)
-- =
--   (*) `on` f . g

--   flip on f . flip on g
-- =
--   (\h (*) -> (*) `on` h) f . (\h (*) -> (*) `on` h) g
-- =
--   (\(*) -> (*) `on` f) . (\(*) -> (*) `on` g)
-- =
--   \(*) -> (*) `on` g `on` f
-- = { See above. }
--   \(*) -> (*) `on` g . f
-- =
--   (\h (*) -> (*) `on` h) (g . f)
-- =
--   flip on (g . f)

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(.*.) `on` f = \x y -> f x .*. f y

