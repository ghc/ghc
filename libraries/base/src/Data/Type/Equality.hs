{-# LANGUAGE Safe #-}

{-# LANGUAGE ExplicitNamespaces #-}

-- |
--
-- Module      :  Data.Type.Equality
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  not portable
--
-- Definition of propositional equality @(':~:')@. Pattern-matching on a variable
-- of type @(a ':~:' b)@ produces a proof that @a '~' b@.
--
-- @since 4.7.0.0

module Data.Type.Equality
    (-- *  The equality types
     type (~),
     type (~~),
     (:~:)(..),
     (:~~:)(..),
     -- *  Working with equality
     sym,
     trans,
     castWith,
     gcastWith,
     apply,
     inner,
     outer,
     -- *  Inferring equality from other types
     TestEquality(..),
     -- *  Boolean type-level equality
     type (==)
     ) where

import GHC.Internal.Data.Type.Equality
