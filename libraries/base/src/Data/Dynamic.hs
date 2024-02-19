{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Data.Dynamic
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The Dynamic interface provides basic support for dynamic types.
--
-- Operations for injecting values of arbitrary type into
-- a dynamically typed value, Dynamic, are provided, together
-- with operations for converting dynamic values into a concrete
-- (monomorphic) type.
--

module Data.Dynamic
    (-- *  The @Dynamic@ type
     Dynamic(..),
     -- *  Converting to and from @Dynamic@
     toDyn,
     fromDyn,
     fromDynamic,
     -- *  Applying functions of dynamic type
     dynApply,
     dynApp,
     dynTypeRep,
     -- *  Convenience re-exports
     Typeable
     ) where

import GHC.Internal.Data.Dynamic