{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Type.Reflection
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2017
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable (requires GADTs and compiler support)
--
-- This provides a type-indexed type representation mechanism, similar to that
-- described by,
--
-- * Simon Peyton-Jones, Stephanie Weirich, Richard Eisenberg,
-- Dimitrios Vytiniotis. "<https://www.microsoft.com/en-us/research/wp-content/uploads/2016/08/dynamic.pdf A reflection on types>".
-- /Proc. Philip Wadler's 60th birthday Festschrift/, Edinburgh (April 2016).
--
-- The interface provides 'I.TypeRep', a type representation which can
-- be safely decomposed and composed. See "Data.Dynamic" for an example of this.
--
-- @since 4.10.0.0
--
-----------------------------------------------------------------------------
module Type.Reflection
    ( -- * The Typeable class
      I.Typeable
    , I.typeRep
    , I.withTypeable

      -- * Propositional equality
    , (:~:)(Refl)
    , (:~~:)(HRefl)

      -- * Type representations
      -- ** Type-Indexed
    , I.TypeRep
    , pattern I.TypeRep
    , I.typeOf
    , pattern I.App, pattern I.Con, pattern I.Con', pattern I.Fun
    , I.typeRepTyCon
    , I.rnfTypeRep
    , I.eqTypeRep
    , I.typeRepKind
    , I.splitApps

      -- ** Quantified
      --
      -- "Data.Typeable" exports a variant of this interface (named differently
      -- for backwards compatibility).
    , I.SomeTypeRep(..)
    , I.someTypeRep
    , I.someTypeRepTyCon
    , I.rnfSomeTypeRep

      -- * Type constructors
    , I.TyCon           -- abstract, instance of: Eq, Show, Typeable
                        -- For now don't export Module, to avoid name clashes
    , I.tyConPackage
    , I.tyConModule
    , I.tyConName
    , I.rnfTyCon

      -- * Module names
    , I.Module
    , I.moduleName, I.modulePackage, I.rnfModule
    ) where

import qualified Data.Typeable.Internal as I
import Data.Type.Equality
