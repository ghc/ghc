{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.ST
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'ST' Monad.
--

module GHC.ST
    (ST(..),
     STret(..),
     STRep,
     runST,
     -- *  Unsafe functions
     liftST,
     unsafeInterleaveST,
     unsafeDupableInterleaveST
     ) where

import GHC.Internal.ST
