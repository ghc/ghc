{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Data.Typeable
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The 'Typeable' class reifies types to some extent by associating type
-- representations to types. These type representations can be compared,
-- and one can in turn define a type-safe cast operation. To this end,
-- an unsafe cast is guarded by a test for type (representation)
-- equivalence. The module "Data.Dynamic" uses Typeable for an
-- implementation of dynamics. The module "Data.Data" uses Typeable
-- and type-safe cast (but not dynamics) to support the \"Scrap your
-- boilerplate\" style of generic programming.
--
-- == Compatibility Notes
--
-- Since GHC 8.2, GHC has supported type-indexed type representations.
-- "Data.Typeable" provides type representations which are qualified over this
-- index, providing an interface very similar to the "Typeable" notion seen in
-- previous releases. For the type-indexed interface, see "Type.Reflection".
--
-- Since GHC 7.10, all types automatically have 'Typeable' instances derived.
-- This is in contrast to previous releases where 'Typeable' had to be
-- explicitly derived using the @DeriveDataTypeable@ language extension.
--
-- Since GHC 7.8, 'Typeable' is poly-kinded. The changes required for this might
-- break some old programs involving 'Typeable'. More details on this, including
-- how to fix your code, can be found on the
-- <https://gitlab.haskell.org/ghc/ghc/wikis/ghc-kinds/poly-typeable PolyTypeable wiki page>
--

module Data.Typeable
    (-- *  The Typeable class
     Typeable,
     typeOf,
     typeRep,
     -- *  Propositional equality
     (:~:)(Refl),
     (:~~:)(HRefl),
     -- *  Type-safe cast
     cast,
     eqT,
     heqT,
     decT,
     hdecT,
     gcast,
     -- *  Generalized casts for higher-order kinds
     gcast1,
     gcast2,
     -- *  A canonical proxy type
     Proxy(..),
     -- *  Type representations
     TypeRep,
     rnfTypeRep,
     showsTypeRep,
     mkFunTy,
     -- *  Observing type representations
     funResultTy,
     splitTyConApp,
     typeRepArgs,
     typeRepTyCon,
     typeRepFingerprint,
     -- *  Type constructors
     TyCon,
     tyConPackage,
     tyConModule,
     tyConName,
     rnfTyCon,
     tyConFingerprint,
     -- *  For backwards compatibility
     typeOf1,
     typeOf2,
     typeOf3,
     typeOf4,
     typeOf5,
     typeOf6,
     typeOf7,
     trLiftedRep
     ) where

import GHC.Internal.Data.Typeable