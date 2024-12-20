{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Data.Coerce
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Safe coercions between data types.
--
-- More in-depth information can be found on the
-- <https://gitlab.haskell.org/ghc/ghc/wikis/roles Roles wiki page>
--
-- @since base-4.7.0.0
-----------------------------------------------------------------------------

module GHC.Internal.Data.Coerce
        ( -- * Safe coercions
          -- @since base-4.7.0.0
          coerce, Coercible
        ) where
import GHC.Internal.Prim (coerce)
import GHC.Internal.Types (Coercible)
