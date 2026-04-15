{-# LANGUAGE Safe #-}

{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- |
--
-- Module      :  GHC.ByteOrder
-- Copyright   :  (c) The University of Glasgow, 1994-2000
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Target byte ordering.
--
-- @since 4.11.0.0

module GHC.ByteOrder
    (ByteOrder(..),
     targetByteOrder
     ) where

import GHC.Internal.ByteOrder

import Text.Read

{-NOTE:
    The following instance is technically an orphan, but practically it is not,
    since ordinary users should not use @ghc-internal@ directly and thus get
    'ByteOrder' only through this module.
-}

-- | @since base-4.11.0.0
deriving instance Read ByteOrder
