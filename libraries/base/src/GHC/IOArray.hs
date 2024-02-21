{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.IOArray
-- Copyright   :  (c) The University of Glasgow 2008
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The IOArray type
--

module GHC.IOArray
    (IOArray(..),
     newIOArray,
     unsafeReadIOArray,
     unsafeWriteIOArray,
     readIOArray,
     writeIOArray,
     boundsIOArray
     ) where

import GHC.Internal.IOArray
