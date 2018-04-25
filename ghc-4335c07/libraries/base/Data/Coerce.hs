{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Coerce
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
-- <https://ghc.haskell.org/trac/ghc/wiki/Roles Roles wiki page>
--
-- @since 4.7.0.0
-----------------------------------------------------------------------------

module Data.Coerce
        ( -- * Safe coercions
          coerce, Coercible,
        ) where
import GHC.Prim (coerce)
import GHC.Types (Coercible)

import GHC.Base () -- for build ordering; see Notes in GHC.Base for more info
