{-# LANGUAGE DerivingStrategies          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.JS.Ident
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
--
-- * Domain and Purpose
--
--     GHC.JS.Ident defines identifiers for the JS backend. We keep this module
--     separate to prevent coupling between GHC and the backend and between
--     unrelated modules is the JS backend.
--
-- * Consumers
--
--     The entire JavaScript Backend consumes this module including modules in
--     GHC.JS.\* and modules in GHC.StgToJS.\*
--
-- * Additional Notes
--
--     This module should be kept as small as possible. Anything added to it
--     will be coupled to the JS backend EDSL and the JS Backend including the
--     linker and rts. You have been warned.
--
-----------------------------------------------------------------------------

module GHC.JS.Ident
  ( Ident(..)
  , name
  ) where

import GHC.Prelude

import GHC.Data.FastString
import GHC.Types.Unique
import GHC.Utils.Outputable

--------------------------------------------------------------------------------
--                            Identifiers
--------------------------------------------------------------------------------

-- | A newtype wrapper around 'FastString' for JS identifiers.
newtype Ident = TxtI { identFS :: FastString }
 deriving stock   (Show, Eq)
 deriving newtype (Uniquable, Outputable)

-- | To give a thing a name is to have power over it. This smart constructor
-- serves two purposes: first, it isolates the JS backend from the rest of GHC.
-- The backend should not explicitly use types provided by GHC but instead
-- should wrap them such as we do here. Second it creates a symbol in the JS
-- backend, but it does not yet give that symbols meaning. Giving the symbol
-- meaning only occurs once it is used with a combinator from @GHC.JS.Make@.
name :: FastString -> Ident
name = TxtI
