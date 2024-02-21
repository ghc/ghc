{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.Ix
-- Copyright   :  (c) The University of Glasgow, 1994-2000
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- GHC\'s Ix typeclass implementation.
--

module GHC.Ix
    (Ix(..),
     indexError
     ) where

import GHC.Internal.Ix
