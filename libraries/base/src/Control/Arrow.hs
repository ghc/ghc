{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Control.Arrow
-- Copyright   :  (c) Ross Paterson 2002
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Basic arrow definitions, based on
--
--  * /Generalising Monads to Arrows/, by John Hughes,
--    /Science of Computer Programming/ 37, pp67-111, May 2000.
--
-- plus a couple of definitions ('returnA' and 'loop') from
--
--  * /A New Notation for Arrows/, by Ross Paterson, in /ICFP 2001/,
--    Firenze, Italy, pp229-240.
--
-- These papers and more information on arrows can be found at
-- <http://www.haskell.org/arrows/>.

module Control.Arrow
    (-- *  Arrows
     Arrow(..),
     Kleisli(..),
     -- **  Derived combinators
     returnA,
     (^>>),
     (>>^),
     (>>>),
     (<<<),
     -- **  Right-to-left variants
     (<<^),
     (^<<),
     -- *  Monoid operations
     ArrowZero(..),
     ArrowPlus(..),
     -- *  Conditionals
     ArrowChoice(..),
     -- *  Arrow application
     ArrowApply(..),
     ArrowMonad(..),
     leftApp,
     -- *  Feedback
     ArrowLoop(..)
     ) where

import GHC.Internal.Control.Arrow
