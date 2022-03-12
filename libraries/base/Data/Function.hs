{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK print-explicit-runtime-reps #-}
  -- Show the levity-polymorphic signature of '$'

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Function
-- Copyright   :  Nils Anders Danielsson 2006
--             ,  Alexander Berntsen     2014
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Simple combinators working solely on and with functions.
--
-----------------------------------------------------------------------------

module Data.Function
  ( -- * "Prelude" re-exports
    id, const, (.), flip, ($)
    -- * Other combinators
  , (&)
  , fix
  , on
  ) where

import GHC.Base ( ($), (.), id, const, flip )

infixl 0 `on`
infixl 1 &

-- | @'fix' f@ is the least fixed point of the function @f@,
-- i.e. the least defined @x@ such that @f x = x@.
--
-- For example, we can write the factorial function using direct recursion as
--
-- >>> let fac n = if n <= 1 then 1 else n * fac (n-1) in fac 5
-- 120
--
-- This uses the fact that Haskell’s @let@ introduces recursive bindings. We can
-- rewrite this definition using 'fix',
--
-- >>> fix (\rec n -> if n <= 1 then 1 else n * rec (n-1)) 5
-- 120
--
-- Instead of making a recursive call, we introduce a dummy parameter @rec@;
-- when used within 'fix', this parameter then refers to 'fix'’s argument, hence
-- the recursion is reintroduced.
fix :: (a -> a) -> a
fix f = let x = f x in x

-- | @'on' b u x y@ runs the binary function @b@ /on/ the results of applying
-- unary function @u@ to two arguments @x@ and @y@. From the opposite
-- perspective, it transforms two inputs and combines the outputs.
--
-- @((+) \``on`\` f) x y = f x + f y@
--
-- Typical usage: @'Data.List.sortBy' ('Prelude.compare' \`on\` 'Prelude.fst')@.
--
-- Algebraic properties:
--
-- * @(*) \`on\` 'id' = (*) -- (if (*) &#x2209; {&#x22a5;, 'const' &#x22a5;})@
--
-- * @((*) \`on\` f) \`on\` g = (*) \`on\` (f . g)@
--
-- * @'flip' on f . 'flip' on g = 'flip' on (g . f)@
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(.*.) `on` f = \x y -> f x .*. f y
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


-- | '&' is a reverse application operator.  This provides notational
-- convenience.  Its precedence is one higher than that of the forward
-- application operator '$', which allows '&' to be nested in '$'.
--
-- >>> 5 & (+1) & show
-- "6"
--
-- @since 4.8.0.0
(&) :: a -> (a -> b) -> b
x & f = f x

-- $setup
-- >>> import Prelude
