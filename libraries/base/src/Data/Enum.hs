{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Data.Enum
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- The 'Enum' and 'Bounded' classes.
--

module Data.Enum
    (Bounded(..),
     Enum(..)
     ) where

import GHC.Internal.Enum
