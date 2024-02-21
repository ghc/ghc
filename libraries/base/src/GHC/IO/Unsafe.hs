{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.IO.Unsafe
-- Copyright   :  (c) The University of Glasgow 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Unsafe IO operations
--

module GHC.IO.Unsafe
    (unsafePerformIO,
     unsafeInterleaveIO,
     unsafeDupablePerformIO,
     unsafeDupableInterleaveIO,
     noDuplicate
     ) where

import GHC.Internal.IO.Unsafe
