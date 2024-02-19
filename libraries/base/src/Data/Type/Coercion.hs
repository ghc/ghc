-- |
--
-- Module      :  Data.Type.Coercion
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  not portable
--
-- Definition of representational equality ('Coercion').
--
-- @since 4.7.0.0

module Data.Type.Coercion
    (Coercion(..),
     coerceWith,
     gcoerceWith,
     sym,
     trans,
     repr,
     TestCoercion(..)
     ) where

import GHC.Internal.Data.Type.Coercion