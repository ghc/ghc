{-# LANGUAGE Safe #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MagicHash #-}

-- |
--
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

module Type.Reflection
    (-- *  The Typeable class
     Typeable,
     typeRep,
     withTypeable,
     -- *  Propositional equality
     (:~:)(Refl),
     (:~~:)(HRefl),
     -- *  Type representations
     -- **  Type-Indexed
     TypeRep,
     pattern TypeRep,
     typeOf,
     pattern App,
     pattern Con,
     pattern Con',
     pattern Fun,
     typeRepTyCon,
     rnfTypeRep,
     eqTypeRep,
     decTypeRep,
     typeRepKind,
     splitApps,
     -- **  Quantified
     SomeTypeRep(..),
     someTypeRep,
     someTypeRepTyCon,
     rnfSomeTypeRep,
     -- *  Type constructors
     TyCon,
     tyConPackage,
     tyConModule,
     tyConName,
     rnfTyCon,
     -- *  Module names
     Module,
     moduleName,
     modulePackage,
     rnfModule
     ) where

import GHC.Internal.Type.Reflection
