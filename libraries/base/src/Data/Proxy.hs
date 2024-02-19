{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Data.Proxy
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Definition of a Proxy type (poly-kinded in GHC)
--
-- @since 4.7.0.0

module Data.Proxy
    (Proxy(..),
     asProxyTypeOf,
     KProxy(..)
     ) where

import GHC.Internal.Data.Proxy