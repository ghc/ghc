{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK print-explicit-runtime-reps #-}
  -- Show the levity-polymorphic signature of '$'

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Data.Function
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

module GHC.Internal.Data.Function
  ( -- * "Prelude" re-exports
    id, const, (.), flip, ($)
    -- * Other combinators
  , (&)
  , fix
  , on
  , applyWhen
  ) where

import GHC.Internal.Base ( TYPE, ($), (.), id, const, flip )
import GHC.Internal.Data.Bool ( Bool(..) )

infixl 0 `on`
infixl 1 &

-- | @'fix' f@ is the least fixed point of the function @f@,
-- i.e. the least defined @x@ such that @f x = x@.
--
-- When @f@ is strict, this means that because, by the definition of strictness,
-- @f &#x22a5; = &#x22a5;@ and such the least defined fixed point of any strict function is @&#x22a5;@.
--
-- ==== __Examples__
--
-- We can write the factorial function using direct recursion as
--
-- >>> let fac n = if n <= 1 then 1 else n * fac (n-1) in fac 5
-- 120
--
-- This uses the fact that Haskell’s @let@ introduces recursive bindings. We can
-- rewrite this definition using 'fix',
--
-- Instead of making a recursive call, we introduce a dummy parameter @rec@;
-- when used within 'fix', this parameter then refers to 'fix'’s argument, hence
-- the recursion is reintroduced.
--
-- >>> fix (\rec n -> if n <= 1 then 1 else n * rec (n-1)) 5
-- 120
--
-- Using 'fix', we can implement versions of 'GHC.Internal.Data.List.repeat' as @'fix' '.' '(:)'@
-- and 'GHC.Internal.Data.List.cycle' as @'fix' '.' '(++)'@
--
-- >>> take 10 $ fix (0:)
-- [0,0,0,0,0,0,0,0,0,0]
--
-- >>> map (fix (\rec n -> if n < 2 then n else rec (n - 1) + rec (n - 2))) [1..10]
-- [1,1,2,3,5,8,13,21,34,55]
--
-- ==== __Implementation Details__
--
-- The current implementation of 'fix' uses structural sharing
--
-- @'fix' f = let x = f x in x@
--
-- A more straightforward but non-sharing version would look like
--
-- @'fix' f = f ('fix' f)@
fix :: (a -> a) -> a
fix f = let x = f x in x

-- | @'on' b u x y@ runs the binary function @b@ /on/ the results of applying
-- unary function @u@ to two arguments @x@ and @y@. From the opposite
-- perspective, it transforms two inputs and combines the outputs.
--
-- @(op \``on`\` f) x y = f x \``op`\` f y@
--
-- ==== __Examples__
--
-- >>> sortBy (compare `on` length) [[0, 1, 2], [0, 1], [], [0]]
-- [[],[0],[0,1],[0,1,2]]
--
-- >>> ((+) `on` length) [1, 2, 3] [-1]
-- 4
--
-- >>> ((,) `on` (*2)) 2 3
-- (4,6)
--
-- ==== __Algebraic properties__
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
--
-- This is a version of @'flip' 'id'@, where 'id' is specialized from @a -> a@ to @(a -> b) -> (a -> b)@
-- which by the associativity of @(->)@ is @(a -> b) -> a -> b@.
-- flipping this yields @a -> (a -> b) -> b@ which is the type signature of '&'
--
-- ==== __Examples__
--
-- >>> 5 & (+1) & show
-- "6"
--
-- >>> sqrt $ [1 / n^2 | n <- [1..1000]] & sum & (*6)
-- 3.1406380562059946
--
-- @since base-4.8.0.0
(&) :: forall r a (b :: TYPE r). a -> (a -> b) -> b
x & f = f x

-- | 'applyWhen' applies a function to a value if a condition is true,
-- otherwise, it returns the value unchanged.
--
-- It is equivalent to @'flip' ('GHC.Internal.Data.Bool.bool' 'id')@.
--
-- ==== __Examples__
--
-- >>> map (\x -> applyWhen (odd x) (*2) x) [1..10]
-- [2,2,6,4,10,6,14,8,18,10]
--
-- >>> map (\x -> applyWhen (length x > 6) ((++ "...") . take 3) x) ["Hi!", "This is amazing", "Hope you're doing well today!", ":D"]
-- ["Hi!","Thi...","Hop...",":D"]
--
-- ==== __Algebraic properties__
--
-- * @applyWhen 'True' = 'id'@
--
-- * @applyWhen 'False' f = 'id'@
--
-- @since base-4.18.0.0
applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True  f x = f x
applyWhen False _ x = x
-- Proofs:
--
-- flip bool id = \q f -> bool id f q
-- = \q f -> case q of
--     True -> f = \x -> f x
--     False -> id = \x -> x ∎
--
-- applyWhen True = \f x -> f x
-- = \f -> \x -> f x = \f -> f = id ∎
--
-- applyWhen False f = \x -> x = id ∎

-- $setup
-- >>> import Prelude
