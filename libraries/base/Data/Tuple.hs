{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- XXX -fno-warn-unused-imports needed for the GHC.Tuple import below. Sigh.
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tuple
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The tuple data types, and associated functions.
--
-----------------------------------------------------------------------------

module Data.Tuple
  ( fst         -- :: (a,b) -> a
  , snd         -- :: (a,b) -> a
  , curry       -- :: ((a, b) -> c) -> a -> b -> c
  , uncurry     -- :: (a -> b -> c) -> ((a, b) -> c)
#ifdef __NHC__
  , (,)(..)
  , (,,)(..)
  , (,,,)(..)
  , (,,,,)(..)
  , (,,,,,)(..)
  , (,,,,,,)(..)
  , (,,,,,,,)(..)
  , (,,,,,,,,)(..)
  , (,,,,,,,,,)(..)
  , (,,,,,,,,,,)(..)
  , (,,,,,,,,,,,)(..)
  , (,,,,,,,,,,,,)(..)
  , (,,,,,,,,,,,,,)(..)
  , (,,,,,,,,,,,,,,)(..)
#endif
  )
    where

#ifdef __GLASGOW_HASKELL__
import GHC.Bool
import GHC.Classes
import GHC.Ordering
-- XXX The standalone deriving clauses fail with
--     The data constructors of `(,)' are not all in scope
--       so you cannot derive an instance for it
--     In the stand-alone deriving instance for `Eq (a, b)'
-- if we don't import GHC.Tuple
import GHC.Tuple
#endif  /* __GLASGOW_HASKELL__ */

#ifdef __NHC__
import Prelude
import Prelude
  ( (,)(..)
  , (,,)(..)
  , (,,,)(..)
  , (,,,,)(..)
  , (,,,,,)(..)
  , (,,,,,,)(..)
  , (,,,,,,,)(..)
  , (,,,,,,,,)(..)
  , (,,,,,,,,,)(..)
  , (,,,,,,,,,,)(..)
  , (,,,,,,,,,,,)(..)
  , (,,,,,,,,,,,,)(..)
  , (,,,,,,,,,,,,,)(..)
  , (,,,,,,,,,,,,,,)(..)
  -- nhc98's prelude only supplies tuple instances up to size 15
  , fst, snd
  , curry, uncurry
  )
#endif

default ()              -- Double isn't available yet

#ifdef __GLASGOW_HASKELL__
-- XXX Why aren't these derived?
instance Eq () where
    () == () = True
    () /= () = False

instance Ord () where
    () <= () = True
    () <  () = False
    () >= () = True
    () >  () = False
    max () () = ()
    min () () = ()
    compare () () = EQ

#ifndef __HADDOCK__
deriving instance (Eq  a, Eq  b) => Eq  (a, b)
deriving instance (Ord a, Ord b) => Ord (a, b)
deriving instance (Eq  a, Eq  b, Eq  c) => Eq  (a, b, c)
deriving instance (Ord a, Ord b, Ord c) => Ord (a, b, c)
deriving instance (Eq  a, Eq  b, Eq  c, Eq  d) => Eq  (a, b, c, d)
deriving instance (Ord a, Ord b, Ord c, Ord d) => Ord (a, b, c, d)
deriving instance (Eq  a, Eq  b, Eq  c, Eq  d, Eq  e) => Eq  (a, b, c, d, e)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e) => Ord (a, b, c, d, e)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f)
               => Eq (a, b, c, d, e, f)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f)
               => Ord (a, b, c, d, e, f)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g)
               => Eq (a, b, c, d, e, f, g)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g)
               => Ord (a, b, c, d, e, f, g)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h)
               => Eq (a, b, c, d, e, f, g, h)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h)
               => Ord (a, b, c, d, e, f, g, h)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i)
               => Eq (a, b, c, d, e, f, g, h, i)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i)
               => Ord (a, b, c, d, e, f, g, h, i)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j)
               => Eq (a, b, c, d, e, f, g, h, i, j)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j)
               => Ord (a, b, c, d, e, f, g, h, i, j)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j, Eq k)
               => Eq (a, b, c, d, e, f, g, h, i, j, k)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j, Ord k)
               => Ord (a, b, c, d, e, f, g, h, i, j, k)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j, Eq k, Eq l)
               => Eq (a, b, c, d, e, f, g, h, i, j, k, l)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j, Ord k, Ord l)
               => Ord (a, b, c, d, e, f, g, h, i, j, k, l)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j, Eq k, Eq l, Eq m)
               => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j, Ord k, Ord l, Ord m)
               => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n)
               => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j, Ord k, Ord l, Ord m, Ord n)
               => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                   Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o)
               => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
                   Ord h, Ord i, Ord j, Ord k, Ord l, Ord m, Ord n, Ord o)
               => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
#endif  /* !__HADDOCK__ */
#endif  /* __GLASGOW_HASKELL__ */

-- ---------------------------------------------------------------------------
-- Standard functions over tuples

#if !defined(__HUGS__) && !defined(__NHC__)
-- | Extract the first component of a pair.
fst                     :: (a,b) -> a
fst (x,_)               =  x

-- | Extract the second component of a pair.
snd                     :: (a,b) -> b
snd (_,y)               =  y

-- | 'curry' converts an uncurried function to a curried function.
curry                   :: ((a, b) -> c) -> a -> b -> c
curry f x y             =  f (x, y)

-- | 'uncurry' converts a curried function to a function on pairs.
uncurry                 :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p             =  f (fst p) (snd p)
#endif  /* neither __HUGS__ nor __NHC__ */
