{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.STRef
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- References in the 'ST' monad.
--

module GHC.STRef
    (STRef(..),
     newSTRef,
     readSTRef,
     writeSTRef
     ) where

import GHC.Internal.STRef
