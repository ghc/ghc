-- |
-- Module      :  GHC.StaticPtr
-- Copyright   :  (C) 2014 I/O Tweag
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Symbolic references to values.
--
-- References to values are usually implemented with memory addresses, and this
-- is practical when communicating values between the different pieces of a
-- single process.
--
-- When values are communicated across different processes running in possibly
-- different machines, though, addresses are no longer useful since each
-- process may use different addresses to store a given value.
--
-- To solve such concern, the references provided by this module offer a key
-- that can be used to locate the values on each process. Each process maintains
-- a global table of references which can be looked up with a given key. This
-- table is known as the Static Pointer Table. The reference can then be
-- dereferenced to obtain the value.
--
-- The various communicating processes need to agree on the keys used to refer
-- to the values in the Static Pointer Table, or lookups will fail. Only
-- processes launched from the same program binary are guaranteed to use the
-- same set of keys.
--

module GHC.StaticPtr
  ( StaticPtr
  , deRefStaticPtr
  , StaticKey
  , staticKey
  , unsafeLookupStaticPtr
  , StaticPtrInfo(..)
  , staticPtrInfo
  , staticPtrKeys
  , IsStatic(..)
  ) where

import GHC.Internal.StaticPtr
